#I @".\bin\NFastText"
#r "NFastText.dll"
let supervisedArgs = [|"supervised"; "-input"; "D:/ft/data/dbpedia.train"; "-output"; "D:/ft/result/dbpedia"; "-dim"; "10"; "-lr"; "0.1"; "-wordNgrams"; "2"; "-minCount"; "1"; "-bucket"; "10000000"; "-epoch"; "5"; "-thread"; "4"|]
let testArgs = [|"test";"D:/ft/result/dbpedia.bin";"D:/ft/data/dbpedia.test"|]
let predictArgs = [|"predict";"D:/ft/result/dbpedia.bin";"D:/ft/data/dbpedia.test"|]
let tests = [supervisedArgs; testArgs; predictArgs]
let timeit f timings args  =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    f args |> ignore
    stopWatch.Stop()
    Map.add args stopWatch.Elapsed.TotalSeconds timings

open NFastText
if fsi.CommandLineArgs.Length = 0
then FastTextM.main fsi.CommandLineArgs
else Map.iter <| printfn "%A\n%f" 
              <| List.fold (timeit FastTextM.main) Map.empty tests 
     0
                                               

