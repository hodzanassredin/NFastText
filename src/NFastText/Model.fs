namespace NFastText

module ModelImplementations =
    open Matrix
    open System.Collections.Generic
    
    let negativeTableSize = 10000000

    type ModelState = 
         val Wi : Matrix 
         val Wo : Matrix 
         val isSupModel : bool 
         val Rng : Random.Mcg31m1
         val Grad : Vector
         val Output : Vector
         val Hidden : Vector
         val Osz : int
         val mutable Loss : float32
         val mutable Nexamples : int
         new(wi : Matrix, wo : Matrix, isSupModel, dim, osz, seed : int)= 
            {
                Wi = wi
                Wo = wo
                isSupModel = isSupModel
                Rng = Random.Mcg31m1(seed)
                Grad = createVector(dim)
                Output = createVector(osz)
                Hidden = createVector(dim)
                Osz = osz
                Loss = 0.0f
                Nexamples = 1
            }

         member model.BinaryLogistic(target : int, label : bool, lr : float32)  =
            let dot = model.Wo.DotRow(model.Hidden, target)
            let score = Utils.sigmoid(dot)
            let bf = if label then 1.f else 0.f
            let alpha = lr * (bf - score)
            model.Grad.AddRow(model.Wo, target, alpha)
            model.Wo.AddRow(model.Hidden, target, alpha)
            if label then -Utils.log(score)
            else -Utils.log(1.0f - score)

    type IConcreteModel =
        abstract member FindKBest : int * MinHeap -> Unit
        abstract member GetLoss : int * float32 -> float32

    type SoftmaxModel(model : ModelState) =
        member x.ComputeOutputSoftmax() =
            model.Output.Mul(model.Wo, model.Hidden)
            let mutable maxv = model.Output.[0]
            let mutable z = 0.0f
            for i = 0 to (model.Osz - 1) do
                maxv <- max (model.Output.[i]) maxv
            for i = 0 to (model.Osz - 1) do
                model.Output.[i] <- exp(model.Output.[i] - maxv)
                z <- z + model.Output.[i]
            for i = 0 to (model.Osz - 1) do
                model.Output.[i] <- model.Output.[i] / z
        
        interface IConcreteModel with
            member x.FindKBest(k : int, heap : MinHeap)  =
                x.ComputeOutputSoftmax()
                for i = 0 to model.Osz - 1 do
                    let l = Utils.log(model.Output.[i])
                    if heap.Count = k && l < heap.Front().Key
                    then ()
                    else
                        heap.Add(KeyValuePair(l, i))
                        if heap.Count > k
                        then heap.RemoveBack()
        
            member x.GetLoss(target,lr) =
                model.Grad.Zero()
                x.ComputeOutputSoftmax()
                for i = 0 to (model.Osz - 1) do
                    let label = if i = target then 1.0f else 0.0f
                    let alpha = lr * (label - model.Output.[i])
                    model.Grad.AddRow(model.Wo, i, alpha)
                    model.Wo.AddRow(model.Hidden, i, alpha)
                -Utils.log(model.Output.[target])

    type private Node =
       struct
          val mutable Parent: int
          val mutable Left: int
          val mutable Right: int
          val mutable Count: int64
          val mutable Binary : bool

          new(parent: int, left: int, right: int, count: int64, binary : bool) = 
            { 
                Parent = parent
                Left = left 
                Right = right
                Count = count
                Binary = binary
            }
       end

    type HierarhicalSoftmaxModel(counts : int64[], model : ModelState) =
        let osz = model.Osz
        let tree = ResizeArray<Node>(2 * osz - 1)
        let paths = ResizeArray<ResizeArray<int>>()
        let codes = ResizeArray<ResizeArray<bool>>()
        do
            for i = 0 to 2 * osz - 2 do 
                tree.Add(Node(-1,-1,-1,1000000000000000L, false))
            for i = 0 to osz - 1 do
                let mutable x = tree.[i]
                x.Count <- counts.[i]
                tree.[i] <- x
            let mutable leaf = osz - 1
            let mutable node = osz
            for i = osz to 2 * osz - 2 do
                let mini = Array.zeroCreate 2
                for j = 0 to 1 do
                    if leaf >= 0 && tree.[leaf].Count < tree.[node].Count
                    then mini.[j] <- leaf
                         leaf <- leaf - 1
                    else mini.[j] <- node
                         node <- node + 1
                let mutable x = tree.[i]
                x.Left <- mini.[0]
                x.Right <- mini.[1]
                x.Count <- tree.[mini.[0]].Count + tree.[mini.[1]].Count
                tree.[i] <- x

                let mutable x = tree.[mini.[0]]
                x.Parent <- i
                tree.[mini.[0]] <- x

                let mutable x = tree.[mini.[1]]
                x.Parent <- i
                x.Binary <- true
                tree.[mini.[1]] <- x

            for i = 0 to osz - 1 do
                let path = ResizeArray<int>()
                let code = ResizeArray<bool>()
                let mutable j = i;
                while tree.[j].Parent <> -1 do
                    path.Add(tree.[j].Parent - osz)
                    code.Add(tree.[j].Binary)
                    j <- tree.[j].Parent
                paths.Add(path)
                codes.Add(code)

        let rec dfs(k : int, node : int, score : float32, heap : MinHeap) =
            if heap.Count = k && score < heap.Front().Key 
            then ()
            else if tree.[node].Left = -1 && tree.[node].Right = -1
                 then heap.Add(KeyValuePair(score, node))
                      if heap.Count > k
                      then heap.RemoveBack()
                 else let f = Utils.sigmoid(model.Wo.DotRow(model.Hidden, node - osz))
                      dfs(k, tree.[node].Left, score + Utils.log(1.0f - f), heap) 
                      dfs(k, tree.[node].Right, score + Utils.log(f), heap) 

        interface IConcreteModel with 
            member x.GetLoss(target,lr) =
                  let mutable loss = 0.0f
                  model.Grad.Zero()
                  let binaryCode = codes.[target]
                  let pathToRoot = paths.[target]
                  for i = 0 to (pathToRoot.Count-1) do
                        loss <- loss + model.BinaryLogistic(pathToRoot.[i], binaryCode.[i], lr) 
                  loss

            member x.FindKBest(k : int, heap : MinHeap)  =
                 dfs(k, 2 * model.Osz - 2, 0.0f, heap) 
    //negatives array should be shared between 
    let createNegatives (counts : int64[]) (rng: Random.Mcg31m1)=
        let negatives = ResizeArray<int>(counts.Length)
        let mutable z = 0.0f
        for i = 0 to counts.Length - 1 do
            z <- z + float32(float(counts.[i]) ** 0.5)
        for i = 0 to counts.Length - 1 do
            let c = float32(float(counts.[i]) ** 0.5)
            for j = 0 to int(c * float32(negativeTableSize) / z) - 1 do
                negatives.Add(i)
        negatives.Sort(fun x y -> rng.Next())
        negatives

    type NegativeSamplingModel(negatives : ResizeArray<int>, neg, model : ModelState) =
        inherit SoftmaxModel(model)

        let mutable negpos = 0

        let getNegative(target : int) =
            let mutable negative = 0
            let rec f () = 
                negative <- negatives.[negpos]
                negpos <- (negpos + 1) % negatives.Count
                if target = negative then f()
            f()
            negative

        interface IConcreteModel with 
            member x.GetLoss(target,lr) =
                let mutable loss = 0.0f
                model.Grad.Zero()
                for n = 0 to neg do
                    if n = 0 
                    then loss <- loss + model.BinaryLogistic(target, true, lr) 
                    else loss <- loss + model.BinaryLogistic(getNegative target, false, lr) 
                loss

module Model =
    open ModelImplementations
    open Matrix
    open System.Collections.Generic

    type Model(model : ModelState, concrete : IConcreteModel) = 
        let computeHidden (input : int[]) =
            model.Hidden.Zero()
            for i = 0 to input.Length - 1 do
                model.Hidden.AddRow(model.Wi, input.[i])
            model.Hidden.Mul(1.0f / float32(input.Length))

        member x.Rng = model.Rng 
        member x.Loss () = model.Loss / float32(model.Nexamples)
        member x.Predict(input : int[], k : int, arr : ResizeArray<KeyValuePair<float32,int>>)  =
            assert(k > 0)
            let heap = MinHeap(arr) 
            heap.Reserve(k + 1);
            computeHidden input 
            concrete.FindKBest(k, heap)
            arr.Sort(fun x y -> -x.Key.CompareTo(y.Key))

        member x.Update(input : int[], target : int, lr : float32) =
            assert(target >= 0)
            assert(target < model.Osz)
            if input.Length > 0 
            then
                model.Hidden.Zero()
                for i = 0 to input.Length - 1 do
                    model.Hidden.AddRow(model.Wi, input.[i])
                model.Hidden.Mul(1.0f / float32(input.Length))
                model.Loss <- model.Loss + concrete.GetLoss(target,lr)
                model.Nexamples <- model.Nexamples + 1;

                if model.isSupModel
                then model.Grad.Mul(1.0f / float32(input.Length))
                for i = 0 to input.Length - 1 do
                    model.Wi.AddRow(model.Grad, input.[i], 1.0f)
    
    type ModelSharedState = 
        | Negatives of ResizeArray<int> * int
        | Hierarchical of int64[]
        | Softmax of Unit

    let createModel model sharedState =
        let c : ModelImplementations.IConcreteModel = 
                match sharedState with
                    | Negatives(negatives, neg) -> upcast NegativeSamplingModel(negatives, neg, model)
                    | Hierarchical(counts) -> upcast HierarhicalSoftmaxModel(counts, model)
                    | Softmax() -> upcast SoftmaxModel(model)
        Model(model, c)


