open NFastText
open NFastText.FastTextM
open NFastText.FileReader
    
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

module Demo =
    //classification model
    let testDbPedia(loss, expectedPrecision) =
        let trainData = Input.FilePath("D:/ft/data/dbpedia.train")
        let testData = Input.FilePath("D:/ft/data/dbpedia.test")
        let args = {Classifier.args with loss = loss}
        let state = Classifier.train(trainData,4,args,2uy,"__label__",true,None)
        let r = Classifier.test(state, 1, FileReader.streamToLines testData)
        assert(r.precision >= expectedPrecision) 
        assert(r.recall >= expectedPrecision)
        assert(r.nexamples = 70000) 
        let r = Classifier.predict(state, 1, FileReader.streamToLines testData)
        let r =  r |> Seq.take (predictRes.Length)
                   |> Seq.map (List.head >> fst)
                   |> Array.ofSeq
        assert(r = predictRes)

    let testSkipgramWiki() =
        let trainData = Input.FilePath("D:/ft/data/text9")
        let skipgram = Vectorizer.train(trainData,4,Vectorizer.args,Args.VecModel.sg, 3uy, 6uy, true)
        let words = Input.FilePath("D:/ft/data/queries.txt") |> FileReader.streamToWords
        Vectorizer.getWordVectors(skipgram,words)


    let testMy()=
        let corpus = "D:/tmp/fast_text_context.txt_without_stop_morph"
        let getVectors() = 
            let myArgs =  {
                            Vectorizer.args with 
                                epoch = 1
                                dim=10
                                minCount = 1
                          }
            let vecModel = Vectorizer.train(Input.FilePath(corpus + ".train"), 4, myArgs, Args.VecModel.sg,3uy,6uy,true) 
            let words = vecModel.dict_.GetWords() 
            Some(Vectorizer.getWordVectors(vecModel,words)|> Array.ofSeq)

        let vectors = None//getVectors()
        let myArgs = { Classifier.args with
                            epoch = 400
                            dim=10
                            //minCount = 100
                            //lr = 0.05f
                     }

        let trainData = Input.FilePath(corpus + ".train")
        let classificationModel = Classifier.train(trainData,4,myArgs,2uy,"__label__", true, vectors)
        let result = Classifier.test(classificationModel, 1, FileReader.streamToLines trainData)
        printfn "train precision %A" result.precision
        let testData = Input.FilePath(corpus + ".test")
        let result = Classifier.test(classificationModel, 1, FileReader.streamToLines testData)
        printfn "test precision %A" result.precision

[<EntryPoint>]
let main argv = 
    let checks = [|Args.Loss.Softmax,0.98f;Args.Loss.Ns(5),0.97f;Args.Loss.Hs,0.97f|]
    for chk in checks do
        Demo.testDbPedia chk
    Demo.testSkipgramWiki() |> ignore
    Demo.testMy()
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
