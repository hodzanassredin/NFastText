(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
NFastText
======================

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The NFastText library can be <a href="https://nuget.org/packages/NFastText">installed from NuGet</a>:
      <pre>PM> Install-Package NFastText</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

#Description

NFastText is a port of facebook's classification and vectorization tool to .net.

#Data

Samples use data files which could be prepared by this script.




#Classification

This example demonstrates how to train, test and use text classifier.

##Train
Library expects, as an input for training, a text file where every line is a single document.
And every line has to contain one or more labels. Labels are words with a specified prefix.

    __label__spam __label__nsfw enlarge your .... xxx
    __label__not_a_spam __label__conference Fsharp conference next year
    ...

Now we are ready to train. You could use some helpers from a module FileReader to use files and streams as input.

*)
#r "NFastText.dll"
open NFastText
open NFastText.FileReader
let trainData = Input.FilePath("D:/ft/data/dbpedia.train")
//we train our classifier in 4 threads, using 2 wordgrams and default model args, verbosive, without pretrained vectors and with label prefix "__label__"
let state = Classifier.train(trainData, 4, Classifier.args, 2uy, "__label__", true, None)

(**
##Test
Test expects a sequence of lines where line represeted as an array of words.
You could use helper streamToLines
*)
let testData = Input.FilePath("D:/ft/data/dbpedia.test")
let r = Classifier.test(state, 1, FileReader.streamToLines testData)
printfn "%A" r
assert(r.precision >= 9.8f) 
(**
##Predict
Predict expects a sequence of lines where line represeted as an array of words.
It returns a sequence of lists where every list contains k best predictions(labels) with weights. 
*)
let testData = Input.FilePath("D:/ft/data/dbpedia.test")
let k = 1
let r = Classifier.predict(state, k, FileReader.streamToLines testData)
let r =  r |> Seq.head
           |> List.head 
           |> fst
assert(r = "__label__9")

(**
#Vectorization
##Train
Works almost the same as classification, but train files could be without line endings.
*)
let trainData = Input.FilePath("D:/ft/data/text9")
let skipgram = Vectorizer.train(trainData,4,Vectorizer.args,Args.VecModel.sg, 3uy, 6uy, true)
(**
##Vectorization
Expects as input a sequence of words and result is a sequence of tuples of words with associated vectors.
*)
let words = Input.FilePath("D:/ft/data/queries.txt") |> FileReader.streamToWords
let wrodsWithVectors = Vectorizer.getWordVectors(skipgram,words)

(**
##Common tasks
You could save and load trained models
*)
FastTextM.saveState "path" state
let state = FastTextM.loadState "path"
(**

More info
-----------------------

 * [Original FastText project](https://github.com/facebookresearch/fastText) contains papers about how it works.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/hodzanassredin/NFastText/tree/master/docs/content
  [gh]: https://github.com/hodzanassredin/NFastText
  [issues]: https://github.com/hodzanassredin/NFastText/issues
  [readme]: https://github.com/hodzanassredin/NFastText/blob/master/README.md
  [license]: https://github.com/hodzanassredin/NFastText/blob/master/LICENSE.txt
*)


