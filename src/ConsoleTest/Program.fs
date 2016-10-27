// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NFastText
open NFastText.FastTextM
open System.IO
open NFastText.Dictionary
let streamToWords showLineEndings (s:Stream) =
        let r = new StreamReader(s)
        let splitters = [|' '; '\t'; '\n'; '\v'; '\f'; '\r'|]
        let word = ResizeArray<char>()
        let buffer = Array.zeroCreate 1000000
        let mutable readedChars = buffer.Length
        seq{
            while readedChars = buffer.Length do
                readedChars <- r.ReadBlock(buffer, 0, buffer.Length)
                for i = 0  to readedChars - 1 do
                    let char = buffer.[i]
                    if Array.contains char splitters 
                    then if word.Count > 0 
                            then yield System.String.Join("", word)
                                 word.Clear()
                         if showLineEndings && char = '\n'
                         then yield "\n"
                    else word.Add(char)
            if word.Count > 0 then yield System.String.Join("", word)
        }

let maxLineSize = 1024
let rec streamToLines model (s:Stream) fromStartOnEof = 
    let maxLineSize = if model <> Args.model_name.sup
                      then maxLineSize
                      else System.Int32.MaxValue
    
    let rec loop() =
        let words = (streamToWords true s).GetEnumerator()
        seq{
            let line = ResizeArray<_>()
            while words.MoveNext() do
                let word = words.Current
                if word <> "\n" then line.Add(word)
                if (word = "\n" && line.Count > 0) || line.Count = maxLineSize
                then yield line.ToArray()
                     line.Clear()

            if fromStartOnEof 
            then s.Position <- 0L
                 yield! loop()
        }
    loop()

let getTextVectors modelPath dataPath label verbose =
    let state = FastTextM.loadState(modelPath,label,verbose)
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    assert(state.args_.model = Args.model_name.sup)
    let rng = Random.Mcg31m1()
    let src = streamToLines state.args_.model stream false
    let vectors = src |> Seq.map (NFastText.FastTextM.textVector state rng)
    assert(true)

let getWordVectors modelPath dataPath label verbose =
    let state = FastTextM.loadState(modelPath,label,verbose)
    let stream = System.IO.File.Open(dataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    assert(state.args_.model <> Args.model_name.sup)
    let words = streamToWords false stream
    let vectors = words |> Seq.map (NFastText.FastTextM.getVector state)
    let r = Seq.head vectors
    assert(r.Length = state.args_.dim)

let train trainDataPath modelPath threads args label verbose =
    let stream = System.IO.File.Open(trainDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let words = streamToWords false stream
    let dict = Dictionary(args, label, verbose)
    dict.readFromFile(words)
    let state = FastTextM.createState args dict

    let stream = System.IO.File.Open(trainDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = streamToLines state.args_.model stream true

    let state = FastTextM.train state verbose src threads
    FastTextM.saveState (modelPath) state 
    if state.args_.model <> Args.model_name.sup 
    then FastTextM.saveVectors(state, modelPath + ".vec")


let test modelPath testDataPath label verbose =
    let state = FastTextM.loadState(modelPath,label,verbose)

    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = streamToLines state.args_.model stream false
    let sharedState = FastTextM.createSharedState state
    let model = FastTextM.createModel state 1 sharedState
    let r = FastTextM.test(state,model, src,1)
    assert(r.precision >= 0.97f) 
    assert(r.recall >= 0.97f)
    assert(r.nexamples = 70000) 

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
    let state = FastTextM.loadState(modelPath,label,verbose)
    let stream = System.IO.File.Open(testDataPath, FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = streamToLines state.args_.model stream false
    let sharedState = FastTextM.createSharedState state
    let model = FastTextM.createModel state 1 sharedState
    let r = src |> Seq.map (FastTextM.predict state model 1)
    let r = Seq.take (predictRes.Length) r 
                |> Seq.choose id
                |> Seq.map (List.head >> fst)
                |> Array.ofSeq
    assert(r = predictRes)


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
    test "D:/ft/result/dbpedia.bin" "D:/ft/data/dbpedia.test" "__label__" 2
    predict "D:/ft/result/dbpedia.bin" "D:/ft/data/dbpedia.test" "__label__" 2
    //skipgram model
    train "D:/ft/data/text9" "D:/ft/result/text9.bin" 4 skipgramArgs "__label__" 2
    getWordVectors "D:/ft/result/text9.bin" "D:/ft/data/queries.txt" "__label__" 2

    0 // return an integer exit code
