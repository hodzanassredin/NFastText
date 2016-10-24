#I @".\bin\NFastText"
#r "NFastText.dll"

let timeit f =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    f () |> ignore
    stopWatch.Stop()
    stopWatch.Elapsed.TotalSeconds

open NFastText
open NFastText.FastTextM
open System.IO
let streamToWords (s:Stream) =
        let r = new StreamReader(s)
        seq{
        let mutable line = r.ReadLine()
        while line <> null do
            yield! line.Split([|' '; '\t'; '\n'; '\v'; '\f'; '\r'|])
            line <- r.ReadLine()
        }

let split length (xs: 'T[]) =
    let rec loop (xs: 'T[]) =
        seq{
            if xs.Length <= length 
            then yield xs
            else yield xs.[..length-1]
                 yield! loop xs.[length..]
        }
    loop xs

let MAX_LINE_SIZE = 1024
let rec streamToLines model (s:Stream) fromStartOnEof = 
    let r = new StreamReader(s)
    let rec loop() =
        let max_line_size = if model <> Args.model_name.sup
                            then MAX_LINE_SIZE
                            else System.Int32.MaxValue
        seq{
                let mutable line = r.ReadLine()
                while line <> null do
                    let lnWords = line.Split([|' '; '\t'; '\n'; '\v'; '\f'; '\r'|])
                    for chunk in split max_line_size lnWords do
                        yield chunk 
                    line <- r.ReadLine()

                if fromStartOnEof 
                then s.Position <- 0L
                     yield! loop()
                }
    loop()

let getVectors state rng (stream:Stream) =
        use cin = System.Console.OpenStandardInput()
        if state.args_.model = Args.model_name.sup 
        then let src = streamToLines state.args_.model stream false
             src |> Seq.map (NFastText.FastTextM.textVector state rng)
        else let words = streamToWords stream
             words |> Seq.map (NFastText.FastTextM.getVector state)

let trainArgs = {
        input = "D:/ft/data/dbpedia.train"
        output = "D:/ft/result/dbpedia"
        args = { Args.supervisedArgs with
                    dim=10
                    lr = 0.1f
                    wordNgrams = 2
                    minCount = 1
                    bucket = 10000000
                    epoch = 5
               }
        thread =  4
}
let label = "__label__"
let verbose = 2
let train() =
    let output = "D:/ft/result/dbpedia"
    let state = FastTextM.createState label verbose
    let stream = System.IO.File.Open(trainArgs.input, FileMode.Open, FileAccess.Read, FileShare.Read)
    let words = streamToWords stream
    let state, _ = FastTextM.train state label verbose words trainArgs streamToLines
    FastTextM.saveState (output + ".bin") state 
    if state.args_.model <> Args.model_name.sup 
    then FastTextM.saveVectors(state, output)

//    let getVectors model (fasttext : FastText) =
//        fasttext.loadModel(model)
//        fasttext.getVectors()



let test() =
    let state = FastTextM.loadState("D:/ft/result/dbpedia.bin",label,verbose)

    let stream = System.IO.File.Open("D:/ft/data/dbpedia.test", FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = streamToLines state.args_.model stream false
    let model = FastTextM.createModel state 1
    let r = FastTextM.test(state,model, src,1)
    printf "precision %f" r.precision
    printf "recall %f" r.recall
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


let predict() =
    let state = FastTextM.loadState("D:/ft/result/dbpedia.bin",label,verbose)

    let stream = System.IO.File.Open("D:/ft/data/dbpedia.test", FileMode.Open, FileAccess.Read, FileShare.Read)
    let src = streamToLines state.args_.model stream false
    let model = FastTextM.createModel state 1
    let r = src |> Seq.map (FastTextM.predict state model 1)
    let r = Seq.take (predictRes.Length) r 
                |> Seq.choose id
                |> Seq.map (List.head >> fst)
                |> Array.ofSeq
    assert(r = predictRes)

[|"train", train; "test", test; "predict", predict|] 
        |> Array.map (fun (n,f) -> printfn "exec %s" n
                                   n, timeit f)
        |> Array.iter (fun (n,t) -> printfn "%s\n%f" n t)



