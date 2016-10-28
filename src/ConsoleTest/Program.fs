// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NFastText
open NFastText.FastTextM
open System.IO
open NFastText.Dictionary

let maxWordsChunkSize = 1024 

let getTextVectors modelPath dataPath label verbose =
    let state = FastTextM.loadState(modelPath,label,verbose)
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    assert(state.args_.model = Args.model_name.sup)
    let rng = Random.Mcg31m1()
    let src = FileReader.streamToWordsChunks maxWordsChunkSize stream 
    let vectors = src |> Seq.map (NFastText.FastTextM.textVector state rng)
    assert(true)

let getWordVectors modelPath dataPath label verbose =
    let state = FastTextM.loadState(modelPath,label,verbose)
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    assert(state.args_.model <> Args.model_name.sup)
    let words = FileReader.streamToWords stream
    let vectors = words |> Seq.map (NFastText.FastTextM.getVector state)
    let r = Seq.head vectors
    assert(r.Length = state.args_.dim)

let train trainDataPath modelPath threads args label verbose =
    printfn "started training %s" trainDataPath
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
    let state = FastTextM.train state verbose src threads
    FastTextM.saveState (modelPath) state 
    if state.args_.model <> Args.model_name.sup 
    then FastTextM.saveVectors(state, modelPath + ".vec")


let test modelPath testDataPath label verbose =
    printfn "started test %s" modelPath
    let state = FastTextM.loadState(modelPath,label,verbose)
    printfn "args %A" state.args_
    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = FileReader.streamToLines stream 
    let sharedState = FastTextM.createSharedState state
    let model = FastTextM.createModel state 1 sharedState
    let r = FastTextM.test(state,model, src,1)
    printfn "test result %A" r
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


let predict modelPath testDataPath label verbose =
    printfn "started predict %s" modelPath
    let state = FastTextM.loadState(modelPath,label,verbose)
    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = FileReader.streamToLines stream 
    let sharedState = FastTextM.createSharedState state
    let model = FastTextM.createModel state 1 sharedState
    let r = src |> Seq.map (FastTextM.predict state model 1)
               |> Seq.choose id
    printfn "predict first result %A" (Seq.head r)
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
    train "D:/ft/data/dbpedia.train" "D:/ft/result/dbpedia.bin" 4 trainArgs "__label__" 2
    let r = test "D:/ft/result/dbpedia.bin" "D:/ft/data/dbpedia.test" "__label__" 2  
    assert(r.precision >= 0.97f) 
    assert(r.recall >= 0.97f)
    assert(r.nexamples = 70000) 
    let r = predict "D:/ft/result/dbpedia.bin" "D:/ft/data/dbpedia.test" "__label__" 2
    let r = Seq.take (predictRes.Length) r 
                |> Seq.map (List.head >> fst)
                |> Array.ofSeq
    assert(r = predictRes)

    //our models
    //100*10->0.75 bs
    //200*10->0.84 
    //400*10->0.89
    //400*20->0.89
    let lossFns = [Args.LossName.softmax;Args.LossName.hs;Args.LossName.ns]
    let tryLoss loss =
        let myArgs = {trainArgs with epoch = 800; dim=10; loss = loss}
        train "D:/tmp/fast_text_brand_safety.txt.train" "D:/ft/result/bs.bin" 4 myArgs "__label__" 2
        test "D:/ft/result/bs.bin" "D:/tmp/fast_text_brand_safety.txt.test" "__label__" 2
    let results = lossFns |> List.map (fun x -> x, tryLoss x)
    //skipgram model
    train "D:/ft/data/text9" "D:/ft/result/text9.bin" 4 skipgramArgs "__label__" 2
    getWordVectors "D:/ft/result/text9.bin" "D:/ft/data/queries.txt" "__label__" 2

    0 // return an integer exit code
