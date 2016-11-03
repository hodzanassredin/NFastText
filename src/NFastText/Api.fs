namespace NFastText

module Common =
    let train trainInput threads (args:Args.Args) label (verbose:bool) pretrainedWordVectors =
        let words = FileReader.streamToWords trainInput
        let dict = match args.model with
                    | Args.Classifier(wordNGrams) ->  Dictionary(args.common.minCount, 0uy, 0uy, wordNGrams, args.common.bucket, false, args.common.t, label, verbose)
                    | Args.Vectorizer(_, minn, maxn) ->  Dictionary(args.common.minCount, minn, maxn, 0uy, args.common.bucket, true, args.common.t, label, verbose)
        dict.ReadFromFile(words)
        let state = FastTextM.createState args dict

        let mapper = match state.args_.model with 
                        | Args.Classifier(_) -> FileReader.streamToLines
                        | Args.Vectorizer(_) -> FileReader.streamToWordsChunks 
        let src = FileReader.split threads trainInput |> List.map (FileReader.infinite mapper) 
        FastTextM.train state verbose src pretrainedWordVectors


module Classifier =
    let args = { Args.empty with
                        loss  = Args.Loss.Softmax
                        dim=10
                        lr = 0.1f
                        minCount = 1
                        bucket = 10000000
                        epoch = 5
               }

    let train(trainInput,threads,args:Args.CommonArgs,wordNgrams,label,verbose,pretrainedWordVectors) =
        let args = {
            Args.Args.common = args
            Args.Args.model = Args.Classifier(wordNgrams)
        }
        
        Common.train trainInput threads args label verbose pretrainedWordVectors
    
    let test (state,k,lines)  =
        let model = FastTextM.createModel state 1 None
        let r = FastTextM.test(state,model, lines,k)
        r

    let predict(state,k,lines) =
        let model = FastTextM.createModel state 1 None
        let r = lines |> Seq.map (FastTextM.predict state model k)
                      |> Seq.choose id
        r

    let getTextVectors (state: FastTextM.FastTextState, input)  =
        match state.args_.model with Args.Vectorizer(_) -> failwith "cant vectorize text with vectorizer, use classifier" | _ -> ()
        let rng = Mcg31m1()
        let src = FileReader.streamToWordsChunks input 
        Seq.map (NFastText.FastTextM.textVector state rng) src


module Vectorizer=
    let args = { Args.empty with 
                        lr = 0.025f
                        dim = 100
                        ws = 5
                        epoch = 1
                        minCount = 5
                        bucket = 2000000
                        t = 1e-4f
                        lrUpdateRate = 100
               }

    let train(trainInput,threads, args:Args.CommonArgs, vecModel,minChargramSize,maxChargramSize,verbose) =
        let args = {
            Args.Args.common = args
            Args.Args.model = Args.Vectorizer(vecModel,minChargramSize,maxChargramSize)
        }
        
        Common.train trainInput threads args "__label__" verbose None

    let getWordVectors (state: FastTextM.FastTextState, words) =
        match state.args_.model with Args.Classifier(_) -> failwith "cant vectorize text with classifier, use vectorizer" | _ -> ()
        Seq.map (fun w -> w, NFastText.FastTextM.getVector state w) words

