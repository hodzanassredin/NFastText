namespace NFastText
module Args =
    type Loss = 
             | Hs
             | Ns of neg : int 
             | Softmax

    let lossToInt = function
        | Hs -> 0
        | Softmax -> 1
        | Ns(neg) -> neg + 1
        
    let lossFromInt = function
        | 0 -> Hs
        | 1 -> Softmax
        | neg -> Ns(neg - 1)

    type CommonArgs = {
         //learning rate
         lr  : float32
         //size of word vectors
         dim  : int
         //size of the context window
         ws  : int
         //number of epochs
         epoch  : int
         //minimal number of word occurences
         minCount  : int

         //loss function {ns, hs, softmax}
         loss  : Loss
         //number of buckets
         bucket  : int
         
         //change the rate of updates for the learning rate
         lrUpdateRate  : int
         //sampling threshold
         t  : float32
    }
    type VecModel = cbow=1 | sg = 2

    type ModelArgs = 
        | Classifier of wordNgrams  : byte//max length of word ngram
        | Vectorizer of model : VecModel * minn  : byte * maxn  : byte//min length of char ngram
                                                                      //max length of char ngram
    type Args = {
        common :CommonArgs 
        model : ModelArgs
        }

    let empty = {
         lr  = 0.05f
         dim  = 100
         ws  = 5
         epoch  = 5
         minCount  = 5
         loss  = Loss.Ns(neg = 5)
         bucket  = 2000000
         lrUpdateRate  = 100
         t  = 1e-4f
        }

    let saveCommon args (out : System.IO.BinaryWriter) = 
          out.Write(args.dim)
          out.Write(args.ws)
          out.Write(args.epoch)
          out.Write(args.minCount)
          out.Write(lossToInt(args.loss))
          out.Write(args.bucket)
          out.Write(args.lrUpdateRate)
          out.Write(args.t)

    let save args  (out : System.IO.BinaryWriter) = 
        match args.model with
            | Classifier(wordNgrams) -> out.Write(true)
                                        out.Write(wordNgrams)
                                        saveCommon(args.common) 
            | Vectorizer(model,minn,maxn) ->  out.Write(false)
                                              out.Write(int(model))
                                              out.Write(minn)
                                              out.Write(maxn)
                                              saveCommon(args.common) 

    let loadCommon(inp : System.IO.BinaryReader) : CommonArgs= 
        { empty with
              dim  = inp.ReadInt32()
              ws  = inp.ReadInt32()
              epoch  = inp.ReadInt32()
              minCount  = inp.ReadInt32()
              loss  = lossFromInt <| inp.ReadInt32()
              bucket  = inp.ReadInt32()
              lrUpdateRate  = inp.ReadInt32()
              t  = inp.ReadSingle()
        }

    let load (inp : System.IO.BinaryReader) : Args =
        let isClassifier = inp.ReadBoolean()
        if isClassifier 
        then let wordNgrams = inp.ReadByte()
             let args = loadCommon inp
             { common = args; model = Classifier(wordNgrams)}
        else let model  = LanguagePrimitives.EnumOfValue <| inp.ReadInt32()
             let minn  = inp.ReadByte()
             let maxn  = inp.ReadByte()
             let args = loadCommon inp
             { common = args; model = Vectorizer(model,minn,maxn)}