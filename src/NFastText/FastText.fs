namespace NFastText

module FastTextM =
    open Matrix
    open Dictionary
    open Args
    open Model
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

    type FastText(label : String, verbose : int) =  
        let mutable args_ = Args.defaultArgs
        let mutable dict_ = Dictionary(args_, label, verbose)
        let mutable input_ = Matrix.createNull()
        let mutable output_ = Matrix.createNull()
        let mutable model_ = Model(input_, output_, args_, null)
        let mutable tokenCount = 0L //atomic todo
        let mutable start = Stopwatch.StartNew() // todo clock_t
        new() = FastText(ByteString.fromString "__label__", 2)
        member x.getVector(vec : Vector, word : String) =
          let ngrams = dict_.getNgrams(word)
          vec.Zero()
          for i = 0 to ngrams.Count - 1 do
             vec.AddRow(input_, ngrams.[i])
          if ngrams.Count > 0 
          then vec.Mul(1.0f / float32(ngrams.Count))

        member x.saveVectors(output : string) =
          use ofs = try new System.IO.StreamWriter(output + ".vec") 
                    with ex -> failwith "Error opening file for saving vectors."
          ofs.WriteLine(sprintf "%d %d" (dict_.nwords()) (args_.dim))
          let vec = createVector(args_.dim)
          for i = 0 to dict_.nwords() - 1 do
            let word = dict_.getWord(i)
            x.getVector(vec, word)
            ofs.WriteLine(sprintf "%s %A" (word.ToString()) vec)
          ofs.Close()

        member x.saveModel(filename) =
          use ofs = try binaryWriter(filename) 
                    with ex -> failwith "Model file cannot be opened for saving!"
          Args.save(args_, ofs)
          dict_.save(ofs)
          Matrix.save(input_, ofs)
          Matrix.save(output_, ofs)
          ofs.Close()

        member x.loadModel(filename : string) =
          use ifs = try binaryReader(filename) 
                    with ex -> failwith "Model file cannot be opened for loading!"
          args_ <- Args.load(ifs)
          dict_ <- Dictionary(args_, label, verbose)
          dict_.load(ifs)
          input_ <- Matrix.load(ifs)
          output_ <- Matrix.load(ifs)
          model_ <- Model(input_, output_, args_, 0)
          if args_.model = model_name.sup
          then model_.setTargetCounts(dict_.getCounts(entry_type.label).ToArray())
          else model_.setTargetCounts(dict_.getCounts(entry_type.word).ToArray())
          ifs.Close()

        member x.printInfo(progress : float32, loss : float32, thread : int) =
          let t = float32(start.Elapsed.TotalSeconds) 
          let wst = float32(tokenCount) / t
          let lr = args_.lr * (1.0f - progress)
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
            let boundary = model.rng.DiscrUniformSample(1, args_.ws)
            bow.Clear()
            for c = -boundary to boundary do
              if c <> 0 && w + c >= 0 && w + c < line.Count
              then let ngrams = dict_.getNgrams(line.[w + c])
                   bow.AddRange(ngrams)
            model.update(bow.ToArray(), line.[w], lr)

        member x.skipgram(model : Model, lr : float32, line : ResizeArray<int>) =
          for w = 0 to line.Count - 1 do
            //model.wo.data[7428][99]
            let boundary = model.rng.DiscrUniformSample(1, args_.ws)
            let ngrams = dict_.getNgrams(line.[w])
            for c = -boundary to boundary do
              if c <> 0 && w + c >= 0 && w + c < line.Count
              then model.update(ngrams.ToArray(), line.[w + c], lr);
        
        member x.test(filename : string, k : int) =
          let mutable nexamples = 0
          let mutable nlabels = 0
          let mutable precision = 0.0f
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          use ifs = try new BaseTypes.BinaryReader(filename)
                    with ex -> failwith "Test file cannot be opened!"
          
          while ifs.NotEOF() do
            dict_.getLine(ifs, line, labels, model_.rng) |> ignore
            dict_.addNgrams(line, args_.wordNgrams);
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
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          use ifs = try new BinaryReader(filename)
                    with ex -> failwith "Test file cannot be opened!"
          seq{
              while ifs.NotEOF() do
                dict_.getLine(ifs, line, labels, model_.rng) |> ignore // todo
                dict_.addNgrams(line, args_.wordNgrams)
                if line.Count = 0 
                then yield None
                else
                    let predictions = ResizeArray<KeyValuePair<float32,int>>()
                    model_.predict(line.ToArray(), k, predictions)
                    let mutable res = []
                    for i = 0 to predictions.Count - 1 do
                      if i > 0 then printf " "
                      let l = dict_.getLabel(predictions.[i].Value).ToStr()
                      let prob = exp(predictions.[i].Key)
                      res <- (l,prob) :: res
                    yield Some res
              ifs.Close()
          }

        member x.wordVectors() =
            seq{
                  let vec = createVector(args_.dim)
                  use cin = new BinaryReader(System.Console.OpenStandardInput())
                  let word = String()
                  while cin.NotEOF() do
                    let c = cin.ReadByte()
                    if c = 0uy 
                    then x.getVector(vec, word)
                         yield vec
                         word.Clear()
                    else word.Add(c)
            }

        member x.textVectors() =
            seq{
                  let line = ResizeArray<int>()
                  let labels = ResizeArray<int>()
                  let vec = createVector(args_.dim)
                  use cin = new BinaryReader(System.Console.OpenStandardInput())
                  while cin.NotEOF() do
                    dict_.getLine(cin, line, labels, model_.rng) |> ignore//todo
                    dict_.addNgrams(line, args_.wordNgrams)
                    vec.Zero()
                    for i = 0 to line.Count - 1 do
                      vec.AddRow(input_, line.[i])
                    if line.Count > 0
                    then vec.Mul(1.0f / float32(line.Count))
                    yield vec
            }

        member x.getVectors() =
          if args_.model = model_name.sup 
          then x.textVectors()
          else x.wordVectors()

        member x.trainThread(input : string, threadId : int, thread : int, verbose : int) =
          use ifs = new BinaryReader(input)
          ifs.MoveAbs(int64(threadId) * ifs.Length / int64(thread)) 

          let model = Model(input_, output_, args_, threadId)
          if args_.model = model_name.sup
          then model.setTargetCounts(dict_.getCounts(entry_type.label).ToArray())
          else model.setTargetCounts(dict_.getCounts(entry_type.word).ToArray())

          let ntokens = dict_.ntokens()
          let mutable localTokenCount = 0
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          while tokenCount < int64(args_.epoch * ntokens) do
            let progress = float32(tokenCount) / float32(args_.epoch * ntokens)
            let lr = args_.lr * (1.0f - progress)
            localTokenCount <- localTokenCount + dict_.getLine(ifs, line, labels, model.rng)
            if args_.model = model_name.sup
            then
              dict_.addNgrams(line, args_.wordNgrams)
              x.supervised(model, lr, line, labels)
            else if args_.model = model_name.cbow
            then x.cbow(model, lr, line)
            else if args_.model = model_name.sg
            then x.skipgram(model, lr, line)
            if localTokenCount > args_.lrUpdateRate
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
          args_ <- args.args
          dict_ <- Dictionary(args_, label, verbose)
          use ifs = try new BinaryReader(args.input)
                    with ex -> failwith "Input file cannot be opened!"
          
          dict_.readFromFile(ifs)
          ifs.Close()

          input_ <- Matrix.create(dict_.nwords() + int(args_.bucket), args_.dim)
          if args_.model = model_name.sup
          then output_ <- Matrix.create(dict_.nlabels(), args_.dim)
          else output_ <- Matrix.create(dict_.nwords(), args_.dim)
          input_.Uniform(1.0f / float32(args_.dim))
          output_.Zero()

          start <- Stopwatch.StartNew()
          tokenCount <- 0L
          let threads = ResizeArray<Thread>()
          for i = 0 to args.thread - 1 do
            let t = Thread(fun () -> x.trainThread(args.input,i, args.thread, verbose))
            t.Start()
            threads.Add(t)
          for it in threads do
            it.Join()
          model_ <- Model(input_, output_, args_, 0)

          x.saveModel(args.output + ".bin")
          if args_.model <> model_name.sup 
          then x.saveVectors(args.output)

    let test model test k (fasttext : FastText) =
        fasttext.loadModel(model)
        fasttext.test(test, k)

    let predict model test k (fasttext : FastText) =
        fasttext.loadModel(model)
        fasttext.predict(test, k)

    let getVectors model (fasttext : FastText) =
        fasttext.loadModel(model)
        fasttext.getVectors()

    let train args (fasttext : FastText) =
        fasttext.train args
