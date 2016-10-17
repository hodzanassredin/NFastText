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

    type TrainArgs = {
        input : string
        output : string
        args :Args
        thread : int
    }

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
    let createState label verbose = {
        args_ = Args.defaultArgs
        dict_ = Dictionary(Args.defaultArgs, label, verbose)
        input_ = Matrix.createNull()
        output_ = Matrix.createNull()
    }

    let saveState filename state =
          use ofs = try binaryWriter(filename) 
                    with ex -> failwith "Model file cannot be opened for saving!"
          Args.save(state.args_, ofs)
          state.dict_.save(ofs)
          Matrix.save(state.input_, ofs)
          Matrix.save(state.output_, ofs)
          ofs.Close()

    let loadState(filename : string, label, verbose) =
          use ifs = try binaryReader(filename) 
                    with ex -> failwith "Model file cannot be opened for loading!"
          try
              let args = Args.load(ifs)
              let dict_ = Dictionary(args, label, verbose)
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

    let getVector(state, vec : Vector, word : String) =
          let ngrams = state.dict_.getNgrams(word)
          vec.Zero()
          for i = 0 to ngrams.Count - 1 do
             vec.AddRow(state.input_, ngrams.[i])
          if ngrams.Count > 0 
          then vec.Mul(1.0f / float32(ngrams.Count))

    let saveVectors(state, output : string) =
          use ofs = try new System.IO.StreamWriter(output + ".vec") 
                    with ex -> failwith "Error opening file for saving vectors."
          ofs.WriteLine(sprintf "%d %d" (state.dict_.nwords()) (state.args_.dim))
          let vec = createVector(state.args_.dim)
          for i = 0 to state.dict_.nwords() - 1 do
            let word = state.dict_.getWord(i)
            getVector(state, vec, word)
            ofs.WriteLine(sprintf "%s %A" (word.ToString()) vec)
          ofs.Close()

    let wordVectors(state) =
            seq{
                  let vec = createVector(state.args_.dim)
                  use cin = new BinaryReader(System.Console.OpenStandardInput())
                  for word in cin.readWords() do
                    getVector(state, vec, word)
                    yield vec
            }

    let textVectors state rng=
            seq{
                  let vec = createVector(state.args_.dim)
                  use cin = new BinaryReader(System.Console.OpenStandardInput())
                  for line,_ in state.dict_.getLines(cin, rng, false) do
                    state.dict_.addNgrams(line, state.args_.wordNgrams)
                    vec.Zero()
                    for i = 0 to line.Count - 1 do
                      vec.AddRow(state.input_, line.[i])
                    if line.Count > 0
                    then vec.Mul(1.0f / float32(line.Count))
                    yield vec
            }

    let getVectors state rng =
          if state.args_.model = model_name.sup 
          then textVectors state rng
          else wordVectors state 

    type FastText(state : FastTextState, label, verbose) =  
        let mutable model_ = Model(state.input_, state.output_, state.args_, 1)

        let mutable tokenCount = 0L //atomic todo
        let mutable start = Stopwatch.StartNew() // todo clock_t
        do
            if state.args_.model = model_name.sup
            then model_.setTargetCounts(state.dict_.getCounts(entry_type.label).ToArray())
            else model_.setTargetCounts(state.dict_.getCounts(entry_type.word).ToArray())

        member x.printInfo(progress : float32, loss : float32, thread : int) =
          let t = float32(start.Elapsed.TotalSeconds) 
          let wst = float32(tokenCount) / t
          let lr = state.args_.lr * (1.0f - progress)
          let eta = int(t / progress * (1.f - progress) / float32(thread))
          let etah = eta / 3600
          let etam = (eta - etah * 3600) / 60
          printf "\rProgress: %.1f%%  words/sec/thread: %.0f  lr: %.6f  loss: %.6f  eta: %dh %dm" (100.f * progress) wst lr loss etah etam

        member x.supervised(model : Model, 
                            lr : float32,
                            line : ResizeArray<int>,
                            labels : ResizeArray<int>) =
          if labels.Count = 0 || line.Count = 0 then ()
          else let i = model.rng.DiscrUniformSample(0, labels.Count - 1)
               model.update(line.ToArray(), labels.[i], lr)

        member x.cbow(model : Model, lr : float32, line : ResizeArray<int>) =
          let bow =  ResizeArray<int>()
          for w = 0 to line.Count - 1 do
            let boundary = model.rng.DiscrUniformSample(1, state.args_.ws)
            bow.Clear()
            for c = -boundary to boundary do
              if c <> 0 && w + c >= 0 && w + c < line.Count
              then let ngrams = state.dict_.getNgrams(line.[w + c])
                   bow.AddRange(ngrams)
            model.update(bow.ToArray(), line.[w], lr)

        member x.skipgram(model : Model, lr : float32, line : ResizeArray<int>) =
          for w = 0 to line.Count - 1 do
            //model.wo.data[7428][99]
            let boundary = model.rng.DiscrUniformSample(1, state.args_.ws)
            let ngrams = state.dict_.getNgrams(line.[w])
            for c = -boundary to boundary do
              if c <> 0 && w + c >= 0 && w + c < line.Count
              then model.update(ngrams.ToArray(), line.[w + c], lr);
        
        member x.test(filename : string, k : int) =
          let mutable nexamples = 0
          let mutable nlabels = 0
          let mutable precision = 0.0f

          use ifs = try new BaseTypes.BinaryReader(filename)
                    with ex -> failwith "Test file cannot be opened!"
          
          for line,labels in state.dict_.getLines(ifs, model_.rng, false) do
            state.dict_.addNgrams(line, state.args_.wordNgrams);
            if (labels.Count > 0 && line.Count > 0) 
            then
              let predictions = ResizeArray<KeyValuePair<float32,int>>()
              model_.predict(line.ToArray(), k, predictions)
              for i = 0 to predictions.Count - 1 do
                if labels.Contains(predictions.[i].Value) 
                then precision <- precision + 1.0f
              nexamples <- nexamples + 1
              nlabels <- nlabels + labels.Count
          ifs.Close()
          {
            precision = precision / float32(k * nexamples)
            recall = precision / float32(nlabels)
            nexamples = nexamples
            k  = k
          }

          
        member x.predict(filename : string, k : int) =
          use ifs = try new BinaryReader(filename)
                    with ex -> failwith "Test file cannot be opened!"
          seq{
              for line,_ in state.dict_.getLines(ifs, model_.rng, false) do
                state.dict_.addNgrams(line, state.args_.wordNgrams)
                if line.Count = 0 
                then yield None
                else
                    let predictions = ResizeArray<KeyValuePair<float32,int>>()
                    model_.predict(line.ToArray(), k, predictions)
                    let mutable res = []
                    for i = 0 to predictions.Count - 1 do
                      if i > 0 then printf " "
                      let l = state.dict_.getLabel(predictions.[i].Value).ToStr()
                      let prob = exp(predictions.[i].Key)
                      res <- (l,prob) :: res
                    yield Some res
              ifs.Close()
          }

        member x.trainThread(input : string, threadId : int, thread : int, verbose : int) =
          use ifs = new BinaryReader(input)
          ifs.MoveAbs(int64(threadId) * ifs.Length / int64(thread)) 

          let model = Model(state.input_, state.output_, state.args_, threadId)
          if state.args_.model = model_name.sup
          then model.setTargetCounts(state.dict_.getCounts(entry_type.label).ToArray())
          else model.setTargetCounts(state.dict_.getCounts(entry_type.word).ToArray())

          let ntokens = state.dict_.ntokens()
          let mutable localTokenCount = 0

          let lineSrc = state.dict_.getLines(ifs, model.rng, true).GetEnumerator()
          while tokenCount < int64(state.args_.epoch * ntokens) do
            let progress = float32(tokenCount) / float32(state.args_.epoch * ntokens)
            let lr = state.args_.lr * (1.0f - progress)
            lineSrc.MoveNext() |> ignore
            let line, labels = lineSrc.Current
            localTokenCount <- localTokenCount + line.Count + labels.Count//todo verify
            if state.args_.model = model_name.sup
            then
              state.dict_.addNgrams(line, state.args_.wordNgrams)
              x.supervised(model, lr, line, labels)
            else if state.args_.model = model_name.cbow
            then x.cbow(model, lr, line)
            else if state.args_.model = model_name.sg
            then x.skipgram(model, lr, line)
            if localTokenCount > state.args_.lrUpdateRate
            then
              tokenCount <- tokenCount + int64(localTokenCount)
              localTokenCount <- 0
              if threadId = 0 && verbose > 1
              then x.printInfo(progress, model.getLoss(), thread)
          if threadId = 0 
          then x.printInfo(1.0f, model.getLoss(), thread)
               printfn ""
          ifs.Close()

        member x.train(args : TrainArgs) =
          state.args_ <- args.args
          state.dict_ <- Dictionary(state.args_, label, verbose)
          use ifs = try new BinaryReader(args.input)
                    with ex -> failwith "Input file cannot be opened!"
          let words = ifs.readWords()
          state.dict_.readFromFile(words)
          ifs.Close()

          state.input_ <- Matrix.create(state.dict_.nwords() + int(state.args_.bucket), state.args_.dim)
          if state.args_.model = model_name.sup
          then state.output_ <- Matrix.create(state.dict_.nlabels(), state.args_.dim)
          else state.output_ <- Matrix.create(state.dict_.nwords(), state.args_.dim)
          state.input_.Uniform(1.0f / float32(state.args_.dim))
          state.output_.Zero()

          start <- Stopwatch.StartNew()
          tokenCount <- 0L
          let threads = ResizeArray<Thread>()
          for i = 0 to args.thread - 1 do
            let t = Thread(fun () -> x.trainThread(args.input,i, args.thread, verbose))
            t.Start()
            threads.Add(t)
          for it in threads do
            it.Join()
          model_ <- Model(state.input_, state.output_, state.args_, 0)
          state
          


