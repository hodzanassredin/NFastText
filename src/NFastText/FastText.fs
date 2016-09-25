namespace NFastText

module FastTextM =
    open Matrix
    open Dictionary
    open Args
    open Model
    open System.Collections.Generic
    open System.Diagnostics
    open System.Threading
    type FastText(a : Args) =  
        let mutable args_ = a
        let mutable dict_ = Dictionary(args_)
        let mutable input_ = Matrix.createNull()
        let mutable output_ = Matrix.createNull()
        let mutable model_ = Model(input_, output_, args_, null)
        let mutable tokenCount = 0L //atomic todo
        let mutable start = Stopwatch.StartNew() // todo clock_t
        let EXIT_FAILURE  = 1
        new() = FastText(Args())
        member x.getVector(vec : Vector, word : String) =
          let ngrams = dict_.getNgrams(word)
          vec.Zero()
          for i = 0 to ngrams.Count - 1 do
             vec.AddRow(input_, ngrams.[i])
          if ngrams.Count > 0 
          then vec.Mul(1.0f / float32(ngrams.Count))

        member x.saveVectors() =
          use ofs = try new System.IO.StreamWriter(args_.output + ".vec") 
                    with ex -> failwith "Error opening file for saving vectors."
          ofs.WriteLine(sprintf "%d %d" (dict_.nwords()) (args_.Dim))
          let vec = createVector(args_.Dim)
          for i = 0 to dict_.nwords() - 1 do
            let word = dict_.getWord(i)
            x.getVector(vec, word)
            ofs.WriteLine(sprintf "%s %A" (word.ToString()) vec)
          ofs.Close()

        member x.saveModel(filename) =
          use ofs = try binaryWriter(filename) 
                    with ex -> failwith "Model file cannot be opened for saving!"
          args_.save(ofs)
          dict_.save(ofs)
          Matrix.save(input_, ofs)
          Matrix.save(output_, ofs)
          ofs.Close()

        member x.loadModel(filename : string) =
          use ifs = try binaryReader(filename) 
                    with ex -> failwith "Model file cannot be opened for loading!"
          args_ <- Args()
          dict_ <- Dictionary(args_)
          args_.load(ifs)
          dict_.load(ifs)
          input_ <- Matrix.load(ifs)
          output_ <- Matrix.load(ifs)
          model_ <- Model(input_, output_, args_, 0)
          if args_.model = model_name.sup
          then model_.setTargetCounts(dict_.getCounts(entry_type.label).ToArray())
          else model_.setTargetCounts(dict_.getCounts(entry_type.word).ToArray())
          ifs.Close()

        member x.printInfo(progress : float32, loss : float32) =
          let t = float32(start.Elapsed.TotalSeconds) 
          let wst = float32(tokenCount) / t
          let lr = args_.lr * (1.0f - progress)
          let eta = int(t / progress * (1.f - progress) / float32(args_.thread))
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
          printfn "P@%d:%.3f" k (precision / float32(k * nexamples)) 
          printfn "R@%d:%.3f" k (precision / float32(nlabels))
          printfn "Number of examples: %d" nexamples

        member x.predict(filename : string, k : int, print_prob : bool) =
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          use ifs = try new BinaryReader(filename)
                    with ex -> failwith "Test file cannot be opened!"
          
          while ifs.NotEOF() do
            dict_.getLine(ifs, line, labels, model_.rng) |> ignore // todo
            dict_.addNgrams(line, args_.wordNgrams)
            if line.Count = 0 
            then printfn "n/a"
            else
                let predictions = ResizeArray<KeyValuePair<float32,int>>()
                model_.predict(line.ToArray(), k, predictions)

                for i = 0 to predictions.Count - 1 do
                  if i > 0 then printf " "
                  printf "%s" ( dict_.getLabel(predictions.[i].Value).ToStr())
                  if print_prob then printf " %A" <| exp(predictions.[i].Key)
                printfn ""
          ifs.Close()

        member x.wordVectors() =
          let word = String()
          let vec = createVector(args_.Dim)
          use cin = new BinaryReader(System.Console.OpenStandardInput())
          let word = String()
          while cin.NotEOF() do
            let c = cin.ReadByte()
            if c = 0uy 
            then x.getVector(vec, word)
                 printfn "%s %A" (word.ToString()) vec
                 word.Clear()
            else word.Add(c)

        member x.textVectors() =
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          let vec = createVector(args_.Dim)
          use cin = new BinaryReader(System.Console.OpenStandardInput())
          while cin.NotEOF() do
            dict_.getLine(cin, line, labels, model_.rng) |> ignore//todo
            dict_.addNgrams(line, args_.wordNgrams)
            vec.Zero()
            for i = 0 to line.Count - 1 do
              vec.AddRow(input_, line.[i])
            if line.Count > 0
            then vec.Mul(1.0f / float32(line.Count))
            printfn "%A" vec

        member x.printVectors() =
          if args_.model = model_name.sup 
          then x.textVectors()
          else x.wordVectors()

        member x.trainThread(threadId : int) =
          use ifs = new BinaryReader(args_.input)
          ifs.MoveAbs(int64(threadId) * ifs.Length / int64(args_.thread)) 

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
              if threadId = 0 && args_.verbose > 1
              then x.printInfo(progress, model.getLoss())
          if threadId = 0 
          then x.printInfo(1.0f, model.getLoss())
               printfn ""
          ifs.Close()

        member x.train(args : Args) =
          args_ <- args
          dict_ <- Dictionary(args_)
          use ifs = try new BinaryReader(args_.input)
                    with ex -> failwith "Input file cannot be opened!"
          
          dict_.readFromFile(ifs)
          ifs.Close()

          input_ <- Matrix.create(dict_.nwords() + int(args_.bucket), args_.Dim)
          if args_.model = model_name.sup
          then output_ <- Matrix.create(dict_.nlabels(), args_.Dim)
          else output_ <- Matrix.create(dict_.nwords(), args_.Dim)
          input_.Uniform(1.0f / float32(args_.Dim))
          output_.Zero()

          start <- Stopwatch.StartNew()
          tokenCount <- 0L
          let threads = ResizeArray<Thread>()
          for i = 0 to args_.thread - 1 do
            let t = Thread(fun () -> x.trainThread i)
            t.Start()
            threads.Add(t)
          for it in threads do
            it.Join()
          model_ <- Model(input_, output_, args_, 0)

          x.saveModel(args_.output + ".bin")
          if args_.model <> model_name.sup 
          then x.saveVectors()

    let printUsage() =
        printf "usage: fasttext <command> <args>\n\n"
        printf "The commands supported by fasttext are:\n\n"
        printf "  supervised          train a supervised classifier\n"
        printf "  test                evaluate a supervised classifier\n"
        printf "  predict             predict most likely labels\n"
        printf "  predict-prob        predict most likely labels with probabilities\n"
        printf "  skipgram            train a skipgram model\n"
        printf "  cbow                train a cbow model\n"
        printf "  print-vectors       print vectors given a trained model\n"
        printfn ""

    let printTestUsage() =
        printf "usage: fasttext test <model> <test-data> [<k>]\n\n"
        printf "  <model>      model filename\n"
        printf "  <test-data>  test data filename\n"
        printf "  <k>          (optional; 1 by default) predict top k labels\n"
        printfn ""

    let printPredictUsage() =
        printf "usage: fasttext predict[-prob] <model> <test-data> [<k>]\n\n"
        printf "  <model>      model filename\n"
        printf "  <test-data>  test data filename\n"
        printf "  <k>          (optional; 1 by default) predict top k labels\n"
        printfn ""

    let printPrintVectorsUsage() =
        printf "usage: fasttext print-vectors <model>\n\n"
        printf "  <model>      model filename\n"
        printfn ""
            

    let test(argv : string[]) =
        let k = if  argv.Length = 4 
                then 1
                else if argv.Length = 5
                then int(argv.[4])
                else printTestUsage()
                     failwith ""
        let fasttext = FastText()
        fasttext.loadModel(argv.[2])
        fasttext.test(argv.[3], k)

    let predict(argv : string[]) =
        let k = if argv.Length = 4 then 1
                else if argv.Length = 5 then int(argv.[4])
                else printPredictUsage()
                     failwith("")
        let print_prob = argv.[1] = "predict-prob"
        let fasttext = FastText()
        fasttext.loadModel(argv.[2])
        fasttext.predict(argv.[3], k, print_prob)

    let printVectors(argv : string[]) =
        if argv.Length <> 3
        then printPrintVectorsUsage()
             failwith ""
          
        let fasttext = FastText()
        fasttext.loadModel(argv.[2])
        fasttext.printVectors()

    let train(argv : string[]) =
        let a = Args()
        a.parseArgs(argv)
        let fasttext = FastText()
        fasttext.train(a)

    let main(argv : string[]) =
        try
            let argv = Array.append [|"fastText"|] argv
//            printf "%A" argv
            if argv.Length < 2
            then printUsage();
                 failwith ""
            let command = argv.[1]
            if command = "skipgram" || command = "cbow" || command = "supervised"
            then train(argv)
            else if command = "test" then test(argv)
            else if command = "print-vectors" then printVectors(argv)
            else if command = "predict" || command = "predict-prob" then predict(argv)
            else printUsage()
                 failwith ""
            0
        with ex -> eprintfn "%s" ex.Message
                   1
          

