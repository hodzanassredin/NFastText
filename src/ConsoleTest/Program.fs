// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open NFastText
open NFastText.FastTextM
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
let train() =
    let fs = FastTextM.FastText()
    FastTextM.train trainArgs fs |> ignore

[<EntryPoint>]
let main argv = 
    train()
    0 // return an integer exit code
