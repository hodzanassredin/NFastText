// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NFastText
open NFastText.FastTextM
open System.IO
open NFastText.Dictionary

let maxWordsChunkSize = 1024 

let getTextVectors state dataPath  =
    assert(match state.args_.model with Args.Classifier(_) -> true | _ -> false)
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let rng = Random.Mcg31m1()
    let src = FileReader.streamToWordsChunks maxWordsChunkSize stream 
    Seq.map (NFastText.FastTextM.textVector state rng) src

let loadWordsFromFile dataPath =
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    FileReader.streamToWords stream

let getWordVectors state words =
    assert(match state.args_.model with Args.Vectorizer(_) -> true | _ -> false)
    Seq.map (fun w -> w, NFastText.FastTextM.getVector state w) words

let multiReaders path count mapper =
    [0..count - 1] |> List.map (fun i -> i, System.IO.File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read))
                   |> List.map (fun (i,s) -> s.Position <- s.Length / int64(count) * int64(i)
                                             mapper s)

let train trainDataPath threads (args:Args.Args) label verbose pretrainedWordVectors =
    let stream = System.IO.File.Open(trainDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let words = FileReader.streamToWords stream 
    let dict = match args.model with
                | Args.Classifier(wordNGrams) ->  Dictionary(args.common.minCount, 0uy, 0uy, wordNGrams, args.common.bucket, false, args.common.t, label, verbose)
                | Args.Vectorizer(_, minn, maxn) ->  Dictionary(args.common.minCount, minn, maxn, 0uy, args.common.bucket, true, args.common.t, label, verbose)
    dict.readFromFile(words)
    let state = FastTextM.createState args dict

    let mapper = match state.args_.model with 
                    | Args.Classifier(_) -> FileReader.streamToLines
                    | Args.Vectorizer(_) -> FileReader.streamToWordsChunks maxWordsChunkSize
    let src= multiReaders trainDataPath threads (FileReader.infinite mapper)   
    FastTextM.train state verbose src pretrainedWordVectors

let test state testDataPath =
    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = FileReader.streamToLines stream 
    let model = FastTextM.createModel state 1 None
    let r = FastTextM.test(state,model, src,1)
    r
    
let predictRes = [|
    "__label__9"
    "__label__9"
    "__label__3"
    "__label__6"
    "__label__7"
    "__label__7"
    "__label__11"
    "__label__11"
    "__label__9"
    "__label__13"
    "__label__12"
    "__label__2"
|]


let predict state testDataPath =
    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = FileReader.streamToLines stream 
    let model = FastTextM.createModel state 1 None
    let r = src |> Seq.map (FastTextM.predict state model 1)
                |> Seq.choose id
    r

[<EntryPoint>]
let main argv = 
    //classification model
    let testDbPedia() =
        let state = train "D:/ft/data/dbpedia.train" 4 Args.defaultClassifierAgrs "__label__" 2 None
        let r = test state "D:/ft/data/dbpedia.test" 
        assert(r.precision >= 0.98f) 
        assert(r.recall >= 0.98f)
        assert(r.nexamples = 70000) 
        let r = predict state "D:/ft/data/dbpedia.test" 
        let r =  r |> Seq.take (predictRes.Length)
                   |> Seq.map (List.head >> fst)
                   |> Array.ofSeq
        assert(r = predictRes)
    testDbPedia()
    //skipgram model
    let skipgram = train "D:/ft/data/text9" 4 Args.defaultVetorizerAgrs "__label__" 2 None
    let words = loadWordsFromFile "D:/ft/data/queries.txt" 
    let vecs = getWordVectors skipgram words
    
    //our models
    //100*10->0.75 bs
    //200*10->0.84 
    //400*10->0.89
    //400*10*nongrams->0.90
    //400*20->0.89
    //800*10->0.89

    let corpus = "D:/tmp/fast_text_brand_safety.txt_without_stop_morph"
    let getVectors() = 
        let myArgs =  {Args.defaultVetorizerAgrs with 
                                common = {
                                            Args.defaultVetorizerAgrs.common with 
                                                epoch = 1
                                                dim=10
                                                minCount = 1
                                         }}
        let vecModel = train (corpus + ".train") 4 myArgs "__label__" 2 None
        let words = vecModel.dict_.GetWords() 
        Some(getWordVectors vecModel words|> Array.ofSeq)

    let vectors = getVectors()
    let myArgs = {Args.defaultClassifierAgrs with 
                      common = { Args.defaultClassifierAgrs.common with
                                     epoch = 400
                                     dim=10
                                     //minCount = 100
                                     //lr = 0.05f
                                }
                      model = Args.Classifier(1uy)
                 }
    let classificationModel = train (corpus + ".train") 4 myArgs "__label__" 2 vectors
    let result = test classificationModel (corpus + ".train") 
    printfn "train precision %A" result.precision
    
    let result = test classificationModel (corpus + ".test")
    printfn "test precision %A" result.precision
    System.Console.ReadKey() |> ignore
    

    0 // return an integer exit code
