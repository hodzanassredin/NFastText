#I @"./bin/NFastText"
#r "NFastText.dll"

open NFastText
open NFastText.FileReader

let rootPath = __SOURCE_DIRECTORY__

let testClassifier()=
    //run load_test_data.sh before this script
    let trainData = Input.FilePath(rootPath + "/data/dbpedia.train")
    //we train our classifier in 4 threads, using 2 wordgrams and default model args, verbosive, without pretrained vectors and with label prefix "__label__"
    let state = Classifier.train(trainData, 4, Classifier.args, 2uy, "__label__", true, None)

    let testData = Input.FilePath(rootPath + "/data/dbpedia.test")
    let r = Classifier.test(state, 1, FileReader.streamToLines testData)
    printfn "%A" r
    assert(r.precision >= 9.8f) 

    let k = 1
    let pr = Classifier.predict(state, k, FileReader.streamToLines testData)
    let prf =  pr |> Seq.head
                |> List.head 
                |> fst
    assert(prf = "__label__9")

let testVectorizer() = 

    let vecTrainData = Input.FilePath(rootPath + "/data/text9")
    let skipgram = Vectorizer.train(vecTrainData,4,Vectorizer.args,Args.VecModel.sg, 3uy, 6uy, true)

    let words = Input.FilePath(rootPath + "/data/queries.txt") |> FileReader.streamToWords
    let wordsWithVectors = Vectorizer.getWordVectors(skipgram,words)
    ()
testClassifier()
testVectorizer()

// FastTextM.saveState "path" state
// let state = FastTextM.loadState "path"
