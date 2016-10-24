namespace NFastText
module Args =
    type model_name = cbow=1 | sg = 2 | sup = 3
    type loss_name = hs=1 | ns = 2 | softmax = 3
    type Args = {
         //learning rate
         lr  : float32
         //size of word vectors
         dim  : int
         //size of the context window
         ws  : int
         //number of epochs
         epoch  : int
         minCount  : int
         //number of negatives sampled
         neg  : int
         //max length of word ngram
         wordNgrams  : int
         //loss function {ns, hs, softmax}
         loss  : loss_name
         model : model_name
         //number of buckets
         bucket  : int
         //min length of char ngram
         minn  : int
         //max length of char ngram
         maxn  : int
         //change the rate of updates for the learning rate
         lrUpdateRate  : int
         //sampling threshold
         t  : float32
    }
    let defaultArgs = {
         lr  = 0.05f
         dim  = 100
         ws  = 5
         epoch  = 5
         //minimal number of word occurences
         minCount  = 5
         neg  = 5
         wordNgrams  = 1
         loss  = loss_name.ns
         model  = model_name.sg
         bucket  = 2000000
         minn  = 3
         maxn  = 6
         lrUpdateRate  = 100
         t  = 1e-4f
        }


    let cbowArgs = { defaultArgs with model  = model_name.cbow}

    let validate args =
        if args.wordNgrams  <= 1 && args.maxn  = 0
        then {args with bucket  = 0}
        else args

    let save(args, out : System.IO.BinaryWriter) = 
          out.Write(args.dim)
          out.Write(args.ws)
          out.Write(args.epoch)
          out.Write(args.minCount)
          out.Write(args.neg)
          out.Write(args.wordNgrams)
          out.Write(int(args.loss))
          out.Write(int(args.model))
          out.Write(args.bucket)
          out.Write(args.minn)
          out.Write(args.maxn)
          out.Write(args.lrUpdateRate)
          out.Write(args.t)

    let load(inp : System.IO.BinaryReader) : Args= 
        { defaultArgs with
              dim  = inp.ReadInt32()
              ws  = inp.ReadInt32()
              epoch  = inp.ReadInt32()
              minCount  = inp.ReadInt32()
              neg  = inp.ReadInt32()
              wordNgrams  = inp.ReadInt32()
              loss  = LanguagePrimitives.EnumOfValue <| inp.ReadInt32()
              model  = LanguagePrimitives.EnumOfValue <| inp.ReadInt32()
              bucket  = inp.ReadInt32()
              minn  = inp.ReadInt32()
              maxn  = inp.ReadInt32()
              lrUpdateRate  = inp.ReadInt32()
              t  = inp.ReadSingle()
        }

