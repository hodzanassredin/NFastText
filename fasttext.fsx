#I @"./bin/NFastText"
#r "NFastText.dll"

open NFastText
open NFastText.FileReader
//run load_test_data.sh before this script
let trainData = Input.FilePath("./data/dbpedia.train")
//we train our classifier in 4 threads, using 2 wordgrams and default model args, verbosive, without pretrained vectors and with label prefix "__label__"
let state = Classifier.train(trainData, 4, Classifier.args, 2uy, "__label__", true, None)

let testData = Input.FilePath("D:/ft/data/dbpedia.test")
let r = Classifier.test(state, 1, FileReader.streamToLines testData)
printfn "%A" r
assert(r.precision >= 9.8f) 

let k = 1
let pr = Classifier.predict(state, k, FileReader.streamToLines testData)
let prf =  pr |> Seq.head
              |> List.head 
              |> fst
assert(prf = "__label__9")

let vecTrainData = Input.FilePath("D:/ft/data/text9")
let skipgram = Vectorizer.train(vecTrainData,4,Vectorizer.args,Args.VecModel.sg, 3uy, 6uy, true)

let words = Input.FilePath("D:/ft/data/queries.txt") |> FileReader.streamToWords
let wrodsWithVectors = Vectorizer.getWordVectors(skipgram,words)

// FastTextM.saveState "path" state
// let state = FastTextM.loadState "path"
