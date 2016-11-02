// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NFastText
open NFastText.FastTextM
open System.IO
open NFastText.Dictionary

let maxWordsChunkSize = 1024 

let getTextVectors state dataPath  =
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    assert(state.args_.model = Args.model_name.sup)
    let rng = Random.Mcg31m1()
    let src = FileReader.streamToWordsChunks maxWordsChunkSize stream 
    src |> Seq.map (NFastText.FastTextM.textVector state rng)

let loadWordsFromFile dataPath =
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    FileReader.streamToWords stream

let getWordVectors state words =
    assert(state.args_.model <> Args.model_name.sup)
    words |> Seq.map (fun w -> w, NFastText.FastTextM.getVector state w)

let train trainDataPath threads args label verbose pretrainedWordVectors =
    let stream = System.IO.File.Open(trainDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let words = FileReader.streamToWords stream 
    let dict = Dictionary(args, label, verbose)
    dict.readFromFile(words)
    let state = FastTextM.createState args dict

    let stream = System.IO.File.Open(trainDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let mapper = if state.args_.model = Args.model_name.sup
                  then FileReader.streamToLines
                  else FileReader.streamToWordsChunks maxWordsChunkSize
    let src = FileReader.infinite mapper stream
    FastTextM.train state verbose src threads pretrainedWordVectors

let test state testDataPath =
    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = FileReader.streamToLines stream 
    let sharedState = FastTextM.createSharedState state
    let model = FastTextM.createModel state 1 sharedState
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
    let sharedState = FastTextM.createSharedState state
    let model = FastTextM.createModel state 1 sharedState
    let r = src |> Seq.map (FastTextM.predict state model 1)
                |> Seq.choose id
    r
    
let trainArgs = { Args.defaultArgs with
                    model  = Args.model_name.sup
                    loss  = Args.LossName.softmax
                    minn  = 0
                    maxn  = 0
                    dim=10
                    lr = 0.1f
                    wordNgrams = 2
                    minCount = 1
                    bucket = 10000000
                    epoch = 5
               }

let skipgramArgs = { Args.defaultArgs with 
                        model  = Args.model_name.sg
                        lr = 0.025f
                        dim = 100
                        ws = 5
                        epoch = 1
                        minCount = 5
                        neg = 5
                        loss = Args.LossName.ns
                        bucket = 2000000
                        minn = 3
                        maxn = 6
                        t = 1e-4f
                        lrUpdateRate = 100
                   }

 


[<EntryPoint>]
let main argv = 

    //classification model
//    train "D:/ft/data/dbpedia.train" "D:/ft/result/dbpedia.bin" 4 trainArgs "__label__" 2
//    let r = test "D:/ft/result/dbpedia.bin" "D:/ft/data/dbpedia.test" "__label__" 2  
//    assert(r.precision >= 0.97f) 
//    assert(r.recall >= 0.97f)
//    assert(r.nexamples = 70000) 
//    let r = predict "D:/ft/result/dbpedia.bin" "D:/ft/data/dbpedia.test" "__label__" 2
//    let r = Seq.take (predictRes.Length) r 
//                |> Seq.map (List.head >> fst)
//                |> Array.ofSeq
//    assert(r = predictRes)

    //our models
    //100*10->0.75 bs
    //200*10->0.84 
    //400*10->0.89
    //400*10*nongrams->0.90
    //400*20->0.89
    //800*10->0.89

    let corpus = "D:/tmp/fast_text_brand_safety.txt_without_stop_morph"
    let getVectors() = 
        let myArgs = {skipgramArgs with epoch = 100
                                        dim=100
                                        minCount = 1
                                         }
        let vecModel = train (corpus + ".train") 4 myArgs "__label__" 2 None
        let words = vecModel.dict_.GetWords() 
        Some(getWordVectors vecModel words|> Array.ofSeq)
    let vectors = None//getVectors()
    let myArgs = {trainArgs with epoch = 400
                                 dim=10
                                 wordNgrams = 1
                                 //minCount = 100
                                 //lr = 0.05f
                                 }
    let classificationModel = train (corpus + ".train") 4 myArgs "__label__" 2 vectors
    let result = test classificationModel (corpus + ".train") 
    printfn "train precision %A" result.precision
    
    let result = test classificationModel (corpus + ".test")
    printfn "test precision %A" result.precision
    System.Console.ReadKey() |> ignore
    //skipgram model
    let skipgram = train "D:/ft/data/text9" 4 skipgramArgs "__label__" 2 None
    let words = loadWordsFromFile "D:/ft/data/queries.txt" 
    let vecs = getWordVectors skipgram words

    0 // return an integer exit code
