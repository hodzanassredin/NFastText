namespace NFastText

module FastTextM =
    open Matrix
    open Dictionary
    open Args
    open Model
    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.Threading
    open System.IO

    type TestResult = {
        precision : float32
        recall : float32
        nexamples : int
        k : int
    }

    type FastTextState = {
        mutable args_ : Args
        mutable dict_ : Dictionary
        mutable input_ : Matrix.Matrix
        mutable output_ : Matrix.Matrix
    }
    let createState args dict = {
        args_ = args
        dict_ = dict
        input_ = Matrix.createNull()
        output_ = Matrix.createNull()
    }

    let saveState filename state =
          use ofs = try  new System.IO.BinaryWriter(System.IO.File.Open(filename, System.IO.FileMode.Create))                    
                    with ex -> failwith "Model file cannot be opened for saving!"
          Args.save state.args_ ofs
          state.dict_.save(ofs)
          Matrix.save(state.input_, ofs)
          Matrix.save(state.output_, ofs)
          ofs.Close()

    let loadState(filename : string, label, verbose) =
          use ifs = try let s = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
                        new System.IO.BinaryReader(s)
                    with ex -> failwith "Model file cannot be opened for loading!"
          try
              let args = Args.load(ifs)
              let dict_ = match args.model with
                            | Classifier(wordNGrams) -> Dictionary(args.common.minCount, 0uy, 0uy, wordNGrams, args.common.bucket, false, args.common.t, label, verbose)
                            | Vectorizer(_, minn, maxn) -> Dictionary(args.common.minCount, minn, maxn, 0uy, args.common.bucket, true, args.common.t, label, verbose)

              dict_.load(ifs)
              let input_ = Matrix.load(ifs)
              let output_ = Matrix.load(ifs)
              {
                  args_ = args
                  dict_ = dict_
                  input_ = input_
                  output_ = output_
              }
          finally 
            ifs.Close()

    let getVector state (word : String) =
          let ngrams = state.dict_.getNgrams(word)
          let vec = createVector(state.args_.common.dim)
          for i = 0 to ngrams.Count - 1 do
             vec.AddRow(state.input_, ngrams.[i])
          if ngrams.Count > 0 
          then vec.Mul(1.0f / float32(ngrams.Count))
          vec

    let saveVectors(state, output : string) =
          use ofs = try new System.IO.StreamWriter(output + ".vec") 
                    with ex -> failwith "Error opening file for saving vectors."
          ofs.WriteLine(sprintf "%d %d" (state.dict_.nwords()) (state.args_.common.dim))
          for i = 0 to state.dict_.nwords() - 1 do
            let word = state.dict_.getWord(i)
            let vec = getVector state word
            ofs.WriteLine(sprintf "%s %A" word vec)
          ofs.Close()
    
    let textVector state rng ln =
            let line,_ = state.dict_.mapLine rng ln
            let vec = createVector(state.args_.common.dim)
            state.dict_.addWordNgrams(line)
            vec.Zero()
            for i = 0 to line.Count - 1 do
                vec.AddRow(state.input_, line.[i])
            if line.Count > 0
            then vec.Mul(1.0f / float32(line.Count))
            vec
    
    let createSharedState state =
        let rng = Random.Mcg31m1()
        let tp = match state.args_.model with
                            | Classifier(_) -> entry_type.label
                            | _ -> entry_type.word

        let counts = lazy(state.dict_.getCounts(tp).ToArray())
        match state.args_.common.loss with
            | Args.LossName.hs -> Model.Hierarchical(counts.Value)
            | Args.LossName.ns ->
                            let negatives = ModelImplementations.createNegatives counts.Value rng
                            Model.Negatives(negatives, state.args_.common.neg)
            | Args.LossName.softmax -> Model.Softmax()

    let createModel state seed sharedState = 
        let isSup = match state.args_.model with Classifier(_) -> true | _ -> false
        let model = ModelImplementations.ModelState(state.input_, state.output_, isSup,  state.args_.common.dim, state.output_.m, seed)
        match sharedState with
            | Some(sharedState) -> Model.createModel model sharedState
            | None -> createSharedState state |> Model.createModel model 

    let printInfo(seconds, tokenCount, lr, progress : float32, loss : float32) =
          let t = float32(seconds) 
          let wst = float32(tokenCount) / t
          let lr = lr * (1.0f - progress)
          let eta = int(t / progress * (1.f - progress))
          let etah = eta / 3600
          let etam = (eta - etah * 3600) / 60
          printf "\rProgress: %.1f%%  words/sec/thread: %.0f  lr: %.6f  loss: %.6f  eta: %dh %dm" (100.f * progress) wst lr loss etah etam
          ()

    let supervised(model : Model,
                                 lr : float32,
                                 line : ResizeArray<int>,
                                 labels : ResizeArray<int>)  =
          if labels.Count = 0 || line.Count = 0 then ()
          else let i = model.Rng.DiscrUniformSample(0, labels.Count - 1)
               model.Update(line.ToArray(), labels.[i], lr) 

    let cbow(state, model : Model, lr : float32, line : ResizeArray<int>) =
        let bow =  ResizeArray<int>()
        for w = 0 to line.Count - 1 do
            let boundary = model.Rng.DiscrUniformSample(1, state.args_.common.ws)
            bow.Clear()
            for c = -boundary to boundary do
                if c <> 0 && w + c >= 0 && w + c < line.Count
                then let ngrams = state.dict_.getNgrams(line.[w + c])
                     bow.AddRange(ngrams)
            model.Update(bow.ToArray(), line.[w], lr) 

    let skipgram(state, model : Model, lr : float32, line : ResizeArray<int>) =
        for w = 0 to line.Count - 1 do
            let boundary = model.Rng.DiscrUniformSample(1, state.args_.common.ws)
            let ngrams = state.dict_.getNgrams(line.[w])
            for c = -boundary to boundary do
                if c <> 0 && w + c >= 0 && w + c < line.Count
                then model.Update(ngrams.ToArray(), line.[w + c], lr) 
        
    let test(state, model : Model, lines, k : int) =
        let mutable nexamples = 0
        let mutable nlabels = 0
        let mutable precision = 0.0f
        let lines = lines |> Seq.map (state.dict_.mapLine model.Rng) 
        for line,labels in lines do
            state.dict_.addWordNgrams(line);
            if (labels.Count > 0 && line.Count > 0) 
            then
                let predictions = ResizeArray<KeyValuePair<float32,int>>()
                model.Predict(line.ToArray(), k, predictions) 
                for i = 0 to predictions.Count - 1 do
                    if labels.Contains(predictions.[i].Value) 
                    then precision <- precision + 1.0f
                nexamples <- nexamples + 1
                nlabels <- nlabels + labels.Count
        {
        precision = precision / float32(k * nexamples)
        recall = precision / float32(nlabels)
        nexamples = nexamples
        k  = k
        }

          
    let predict state (model : Model) (k : int) line =
            let line,_ = state.dict_.mapLine model.Rng line
            state.dict_.addWordNgrams(line)
            if line.Count = 0 
            then None
            else
                let predictions = ResizeArray<KeyValuePair<float32,int>>()
                model.Predict(line.ToArray(), k, predictions) 
                let mutable res = []
                for i = 0 to predictions.Count - 1 do
                    if i > 0 then printf " "
                    let l = state.dict_.getLabel(predictions.[i].Value)
                    let prob = exp(predictions.[i].Key)
                    res <- (l,prob) :: res
                Some res
    type Updater = int  -> float32 -> float32 * bool

    let stateUpdater (tokenCount : ref<int64>) ntokens args verbose thread  : Updater =
        let start = Stopwatch.StartNew()
        let mutable localTokenCount = 0
        fun count loss -> 
            localTokenCount <- localTokenCount + count
            let progress = float32(float(!tokenCount) / float(int64(args.epoch) * int64(ntokens)))
            let lr = args.lr * (1.0f - progress)
            if localTokenCount > args.lrUpdateRate
            then tokenCount := !tokenCount + int64(localTokenCount)
                 localTokenCount <- 0
                 if verbose > 1 && thread = 1
                 then printInfo(start.Elapsed.TotalSeconds, !tokenCount, args.lr, progress, loss)
            let finished = !tokenCount >= int64(args.epoch) * int64(ntokens)
            if finished && thread = 1
            then printInfo(start.Elapsed.TotalSeconds, !tokenCount, args.lr, 1.0f, loss)
                 printfn ""
            lr, finished
            

    let worker (tokenCount : ref<int64>) (src:seq<string[]>) state sharedState verbose threadId =
        let up = stateUpdater tokenCount (state.dict_.ntokens()) state.args_.common verbose threadId
        
        let model = createModel state threadId sharedState

        let en = src.GetEnumerator()
        let rec loop lr =
            async{
                en.MoveNext() |> ignore
                let line, labels = state.dict_.mapLine (model.Rng) en.Current
                let count = line.Count + labels.Count
                state.dict_.addWordNgrams(line) 
                match state.args_.model with
                    | Classifier(_) -> supervised(model, lr, line, labels) 
                    | Vectorizer(VecModel.cbow,_,_) -> cbow(state, model, lr, line) 
                    | Vectorizer(VecModel.sg,_,_) -> skipgram(state, model, lr, line) 
                    | _ -> failwith "not supported model"
                let lr, finished = up count (model.Loss())
                if finished 
                then return ()
                else return! loop lr
            }
        loop <| state.args_.common.lr

    let loadVectors state (inp : seq<string * Vector>) = 
        let words = ResizeArray<string>()
        let mat = ResizeArray<float32[]>()
           
        for word,vec in inp do
            words.Add(word)
            state.dict_.add(word)
            if vec.Length <> state.args_.common.dim 
            then failwith "Dimension of pretrained vectors does not match -dim option"
            mat.Add(vec)

        state.dict_.threshold(1L)
        for i = 0 to words.Count - 1 do
            let idx = state.dict_.getId(words.[i])
            if idx < 0 || idx >= state.dict_.nwords() 
            then ()
            else for j = 0 to state.args_.common.dim - 1 do
                     state.input_.data.[idx].[j] <- mat.[i].[j]

    let train state verbose (src: seq<seq<string[]>>) pretrainedVectors =
          
        state.input_ <- Matrix.create(state.dict_.nwords() + int(state.args_.common.bucket), state.args_.common.dim)
        state.input_.Uniform(1.0f / float32(state.args_.common.dim))
          
        let count = match state.args_.model with
            | Classifier(_) -> state.dict_.nlabels()
            | _ -> state.dict_.nwords()
        state.output_ <- Matrix.create(count, state.args_.common.dim)
        state.output_.Zero()

        match pretrainedVectors with
            | Some(xs) -> loadVectors state xs
            | None -> ()
          
        let sharedState = Some(createSharedState state)
        let tokenCount = ref 0L
        src |> Seq.mapi (fun i src -> worker tokenCount src state sharedState verbose i)
            |> Async.Parallel
            |> Async.Ignore 
            |> Async.RunSynchronously
        state

  
    