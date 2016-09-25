namespace NFastText
module Args =
    type model_name = cbow=1 | sg = 2 | sup = 3
    type loss_name = hs=1 | ns = 2 | softmax = 3
    type Args()=
        let mutable lr_ = 0.05f
        let mutable dim_ = 100
        let mutable ws_ = 5
        let mutable epoch_ = 5
        let mutable minCount_ = 5
        let mutable neg_ = 5
        let mutable wordNgrams_ = 1
        let mutable loss_ = loss_name.ns
        let mutable model_ = model_name.sg
        let mutable bucket_ = 2000000
        let mutable minn_ = 3
        let mutable maxn_ = 6
        let mutable thread_ = 12
        let mutable lrUpdateRate_ = 100
        let mutable t_ = 1e-4f
        let mutable label_ = ByteString.fromString("__label__")
        let mutable verbose_ = 2

        let mutable output_ = ""
        let mutable input_ = ""
        let mutable test_ = ""

        member x.Dim = dim_
        member x.Neg = neg_
        member x.loss = loss_
        member x.model = model_
        member x.minn = minn_
        member x.maxn = maxn_
        member x.bucket = bucket_
        member x.label = label_
        member x.minCount = minCount_
        member x.verbose = verbose_
        member x.t = t_
        member x.wordNgrams = wordNgrams_
        member x.output = output_
        member x.lr = lr_
        member x.thread = thread_
        member x.ws = ws_
        member x.epoch = epoch_
        member x.input = input_
        member x.lrUpdateRate = lrUpdateRate_
        member x.test = test_

        member x.parseArgs(argv : string[]) = 
          let command = argv.[1]
          if command = "supervised"
          then
            model_ <- model_name.sup
            loss_ <- loss_name.softmax
            minCount_ <- 1
            minn_ <- 0
            maxn_ <- 0
            lr_ <- 0.1f
          else if command = "cbow"
          then model_ <- model_name.cbow
          let mutable ai = 2
          while ai < argv.Length - 1 do
            if argv.[ai].[0] <> '-'
            then
              printfn "Provided argument without a dash! Usage:"
              x.printHelp()
              failwith ""
            if argv.[ai] = "-h"
            then
              printfn "Here is the help! Usage:" 
              x.printHelp()
              failwith ""
            else if argv.[ai] = "-input" 
            then input_ <- argv.[ai + 1]
            else if argv.[ai] = "-test"
            then test_ <- argv.[ai + 1]
            else if argv.[ai] = "-output"
            then output_ <- argv.[ai + 1]
            else if argv.[ai] = "-lr"
            then lr_ <- float32(argv.[ai + 1]);
            else if argv.[ai] = "-lrUpdateRate" 
            then lrUpdateRate_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-dim"
            then dim_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-ws"
            then ws_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-epoch"
            then epoch_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-minCount"
            then minCount_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-neg"
            then neg_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-wordNgrams"
            then wordNgrams_ <- int(argv.[ai + 1]);
            else if argv.[ai] = "-loss"
                 then
                  if argv.[ai + 1] = "hs" 
                  then loss_ <- loss_name.hs;
                  else if argv.[ai + 1] = "ns" 
                  then loss_ <- loss_name.ns;
                  else if argv.[ai + 1] = "softmax" 
                  then loss_ <- loss_name.softmax;
                  else 
                    printfn "Unknown loss: %s" <| argv.[ai + 1] 
                    x.printHelp();
                    failwith ""
            else if argv.[ai] = "-bucket"
            then bucket_ <- int(argv.[ai + 1])
            else if argv.[ai] = "-minn"
            then minn_ <- int(argv.[ai + 1])
            else if argv.[ai] = "-maxn"
            then maxn_ <- int(argv.[ai + 1])
            else if argv.[ai] = "-thread"
            then thread_ <- int(argv.[ai + 1])
            else if argv.[ai] = "-t"
            then t_ <- float32(argv.[ai + 1])
            else if argv.[ai] = "-label"
            then label_ <- ByteString.fromString(argv.[ai + 1])
            else if argv.[ai] = "-verbose"
            then verbose_ <- int(argv.[ai + 1])
            else 
              printfn "Unknown argument: %s" <| argv.[ai]
              x.printHelp();
              failwith ""
            
            ai <- ai + 2
          if input_ = "" || output_ = "" 
          then
            printfn "Empty input or output path."
            x.printHelp()
            failwith ""
          if wordNgrams_ <= 1 && maxn_ = 0
          then bucket_ <- 0;

        member x.save(out : System.IO.BinaryWriter) = 
          out.Write(dim_)
          out.Write(ws_)
          out.Write(epoch_)
          out.Write(minCount_)
          out.Write(neg_)
          out.Write(wordNgrams_)
          out.Write(int(loss_))
          out.Write(int(model_))
          out.Write(bucket_)
          out.Write(minn_)
          out.Write(maxn_)
          out.Write(lrUpdateRate_)
          out.Write(t_)

        member x.load(inp : System.IO.BinaryReader) = 
          dim_ <- inp.ReadInt32()
          ws_ <- inp.ReadInt32()
          epoch_ <- inp.ReadInt32()
          minCount_ <- inp.ReadInt32()
          neg_ <- inp.ReadInt32()
          wordNgrams_ <- inp.ReadInt32()
          loss_ <- LanguagePrimitives.EnumOfValue <| inp.ReadInt32()
          model_ <- LanguagePrimitives.EnumOfValue <| inp.ReadInt32()
          bucket_ <- inp.ReadInt32()
          minn_ <- inp.ReadInt32()
          maxn_ <- inp.ReadInt32()
          lrUpdateRate_ <- inp.ReadInt32()
          t_ <- inp.ReadSingle()

        member x.printHelp() = 
            printf "\n"
            printf "The following arguments are mandatory:\n"
            printf "  -input        training file path\n"
            printf "  -output       output file path\n\n"
            printf "The following arguments are optional:\n"
            printf "  -lr           learning rate [%f]\n" lr_
            printf "  -lrUpdateRate change the rate of updates for the learning rate [%d]\n" lrUpdateRate_
            printf "  -dim          size of word vectors [%d]\n" dim_
            printf "  -ws           size of the context window [%d]\n" ws_
            printf "  -epoch        number of epochs [%d]\n" epoch_
            printf "  -minCount     minimal number of word occurences [%d]\n" minCount_
            printf "  -neg          number of negatives sampled [%d]\n" neg_
            printf "  -wordNgrams   max length of word ngram [%d]\n" wordNgrams_
            printf "  -loss         loss function {ns, hs, softmax} [ns]\n"
            printf "  -bucket       number of buckets [%d]\n" bucket_
            printf "  -minn         min length of char ngram [%d]\n" minn_
            printf "  -maxn         max length of char ngram [%d]\n" maxn_
            printf "  -thread       number of threads [%d]\n" thread_
            printf "  -t            sampling threshold [%f]\n" t_
            printf "  -label        labels prefix [%s]\n" (label_.ToString())
            printf "  -verbose      verbosity level [%d]\n" verbose_
            printf "\n"


