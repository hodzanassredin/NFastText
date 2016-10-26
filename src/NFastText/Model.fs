namespace NFastText
module Model =
    open Matrix
    open Args
    open System.Collections.Generic
    
    let NEGATIVE_TABLE_SIZE = 10000000
    type Node =
       struct
          val mutable parent: int
          val mutable left: int
          val mutable right: int
          val mutable count: int64
          val mutable binary : bool

          new(parent: int, left: int, right: int, count: int64, binary : bool) = 
            { 
                parent = parent
                left = left 
                right = right
                count = count
                binary = binary
            }
       end

    [<AbstractClass>]
    type Model = {
        wi : Matrix 
        wo : Matrix 
        modelName : model_name 
        rng : Random.Mcg31m1
        grad : Vector
        output : Vector
        hidden : Vector
        osz : int
        mutable loss : float32
        mutable nexamples : int
    }

    let createModel(wi : Matrix, wo : Matrix, args : Args, seed : int)= 
        {
            wi = wi
            wo = wo
            modelName = args.model
            rng = Random.Mcg31m1(seed)
            grad = createVector(args.dim)
            output = createVector(wo.m)
            hidden = createVector(args.dim)
            osz = wo.m
            loss = 0.0f
            nexamples = 1
        }

    let bfloat32 b = if b then 1.f else 0.f
    let binaryLogistic(target : int, label : bool, lr : float32) model =
        let dot = model.wo.DotRow(model.hidden, target)
        let score = Utils.sigmoid(dot)
        let alpha = lr * (bfloat32(label) - score)
        model.grad.AddRow(model.wo, target, alpha)
        model.wo.AddRow(model.hidden, target, alpha)
        if label then -Utils.log(score)
        else -Utils.log(1.0f - score)

    let computeOutputSoftmax model =
        model.output.Mul(model.wo, model.hidden)
        let mutable maxv = model.output.[0]
        let mutable z = 0.0f
        for i = 0 to (model.osz - 1) do
            maxv <- max (model.output.[i]) maxv
        for i = 0 to (model.osz - 1) do
            model.output.[i] <- exp(model.output.[i] - maxv)
            z <- z + model.output.[i]
        for i = 0 to (model.osz - 1) do
            model.output.[i] <- model.output.[i] / z

    let findKBest(k : int, heap : MinHeap) model =
        computeOutputSoftmax model
        for i = 0 to model.osz - 1 do
            let l = Utils.log(model.output.[i])
            if heap.Count = k && l < heap.Front().Key
            then ()
            else
                heap.Add(KeyValuePair(l, i))
                if heap.Count > k
                then heap.RemoveBack()

    let softmax model target lr =
        model.grad.Zero()
        computeOutputSoftmax model
        for i = 0 to (model.osz - 1) do
            let label = if i = target then 1.0f else 0.0f
            let alpha = lr * (label - model.output.[i])
            model.grad.AddRow(model.wo, i, alpha)
            model.wo.AddRow(model.hidden, i, alpha)
        -Utils.log(model.output.[target])

    let computeHidden(input : int[]) model =
        model.hidden.Zero()
        for i = 0 to input.Length - 1 do
            model.hidden.AddRow(model.wi, input.[i])
        model.hidden.Mul(1.0f / float32(input.Length))

    let getLoss model = model.loss / float32(model.nexamples)

    type HSModel = {
        tree : ResizeArray<Node>
        paths : ResizeArray<ResizeArray<int>>
        codes : ResizeArray<ResizeArray<bool>>
    }

    let rec dfs(k : int, node : int, score : float32, heap : MinHeap) (model : HSModel) baseModel =
        if heap.Count = k && score < heap.Front().Key 
        then ()
        else if model.tree.[node].left = -1 && model.tree.[node].right = -1
             then heap.Add(KeyValuePair(score, node))
                  if heap.Count > k
                  then heap.RemoveBack()
             else let f = Utils.sigmoid(baseModel.wo.DotRow(baseModel.hidden, node - baseModel.osz))
                  dfs(k, model.tree.[node].left, score + Utils.log(1.0f - f), heap) model baseModel
                  dfs(k, model.tree.[node].right, score + Utils.log(f), heap) model baseModel

    let hierarchicalSoftmax (model:HSModel) baseModel target lr =
          let mutable loss = 0.0f
          baseModel.grad.Zero()
          let binaryCode = model.codes.[target]
          let pathToRoot = model.paths.[target]
          for i = 0 to (pathToRoot.Count-1) do
                loss <- loss + binaryLogistic(pathToRoot.[i], binaryCode.[i], lr) baseModel
          loss

    type NSModel = {
        negatives : ResizeArray<int>
        mutable negpos : int
        neg : int
    }

    let getNegative(target : int) model =
        let mutable negative = 0
        let rec f () = 
            negative <- model.negatives.[model.negpos]
            model.negpos <- (model.negpos + 1) % model.negatives.Count
            if target = negative then f()
        f()
        negative

    let negativeSampling (model:NSModel) baseModel target lr =
        let mutable loss = 0.0f
        baseModel.grad.Zero()
        for n = 0 to model.neg do
            if n = 0 
            then loss <- loss + binaryLogistic(target, true, lr) baseModel
            else loss <- loss + binaryLogistic(getNegative target model, false, lr) baseModel
        loss

    let createHierarhicalSoftmax (counts : int64[]) osz =
        let tree = ResizeArray<Node>(2 * osz - 1)
        let paths = ResizeArray<ResizeArray<int>>()
        let codes = ResizeArray<ResizeArray<bool>>()
        for i = 0 to 2 * osz - 2 do 
            tree.Add(Node(-1,-1,-1,1000000000000000L, false))
        for i = 0 to osz - 1 do
            let mutable x = tree.[i]
            x.count <- counts.[i]
            tree.[i] <- x
        let mutable leaf = osz - 1
        let mutable node = osz
        for i = osz to 2 * osz - 2 do
            let mini = Array.zeroCreate 2
            for j = 0 to 1 do
                if leaf >= 0 && tree.[leaf].count < tree.[node].count
                then mini.[j] <- leaf
                     leaf <- leaf - 1
                else mini.[j] <- node
                     node <- node + 1
            let mutable x = tree.[i]
            x.left <- mini.[0]
            x.right <- mini.[1]
            x.count <- tree.[mini.[0]].count + tree.[mini.[1]].count
            tree.[i] <- x

            let mutable x = tree.[mini.[0]]
            x.parent <- i
            tree.[mini.[0]] <- x

            let mutable x = tree.[mini.[1]]
            x.parent <- i
            x.binary <- true
            tree.[mini.[1]] <- x

        for i = 0 to osz - 1 do
            let path = ResizeArray<int>()
            let code = ResizeArray<bool>()
            let mutable j = i;
            while tree.[j].parent <> -1 do
                path.Add(tree.[j].parent - osz)
                code.Add(tree.[j].binary)
                j <- tree.[j].parent
            paths.Add(path)
            codes.Add(code)

        {
            codes = codes
            paths = paths
            tree = tree
        }

    let createNegativeSampling(counts: int64[]) (rng:Random.Mcg31m1) neg =
        let negatives = ResizeArray<int>(counts.Length)
        let mutable z = 0.0f
        for i = 0 to counts.Length - 1 do
            z <- z + float32(float(counts.[i]) ** 0.5)
        for i = 0 to counts.Length - 1 do
            let c = float32(float(counts.[i]) ** 0.5)
            for j = 0 to int(c * float32(NEGATIVE_TABLE_SIZE) / z) - 1 do
                negatives.Add(i)
        negatives.Sort(fun x y -> rng.Next())
        {
            negatives = negatives
            negpos = 0
            neg = neg
        }

    type ModelEngine = 
        | HierarhicalSoftmax of HSModel
        | Softmax of Unit 
        | NegativeSampling of NSModel

    

    let createEngine loss (counts: int64[]) rng neg osz =
        match loss with
            | LossName.hs -> createHierarhicalSoftmax counts osz |> HierarhicalSoftmax
            | LossName.ns -> createNegativeSampling counts rng neg |> NegativeSampling
            | LossName.softmax -> Softmax ()

    let getEngineLoss engine model =
        match engine with
            | NegativeSampling(m) -> negativeSampling m model 
            | HierarhicalSoftmax(m) -> hierarchicalSoftmax m model 
            | Softmax() -> softmax model  


    let predict(input : int[], k : int, arr : ResizeArray<KeyValuePair<float32,int>>) engine model =
        assert(k > 0)
        let heap = MinHeap(arr) 
        heap.Reserve(k + 1);
        computeHidden input model
        match engine with
            | HierarhicalSoftmax(m) -> dfs(k, 2 * model.osz - 2, 0.0f, heap) m model
            | _ -> findKBest(k, heap) model
        arr.Sort(fun x y -> -x.Key.CompareTo(y.Key))


    let update(input : int[], target : int, lr : float32) model getLoss =
        assert(target >= 0)
        assert(target < model.osz)
        if input.Length > 0 
        then
            model.hidden.Zero()
            for i = 0 to input.Length - 1 do
                model.hidden.AddRow(model.wi, input.[i])
            model.hidden.Mul(1.0f / float32(input.Length))
            model.loss <- model.loss + getLoss target lr
            model.nexamples <- model.nexamples + 1;

            if model.modelName = model_name.sup
            then model.grad.Mul(1.0f / float32(input.Length))
            for i = 0 to input.Length - 1 do
                model.wi.AddRow(model.grad, input.[i], 1.0f)




        



        



