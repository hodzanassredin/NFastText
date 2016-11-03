namespace NFastText
open System
type IdType = int
type EntryType = Word=0uy | Label=1uy

type private Entry =
    struct
        val mutable word: String
        val mutable count: int64
        val mutable etype: EntryType
        val mutable subwords: ResizeArray<int>
        val mutable binary : bool
        new (word, count, etype, subwords, binary) =
            {word = word; count = count; etype = etype; subwords = subwords; binary = binary}
    end

type Dictionary(minCount, minn, maxn, wordNgrams, bucket, useDiscard, t, label : String, verbose : bool) =
    let getHash(this : System.Collections.Generic.IEnumerable<char>) = 
        let mutable h = 2166136261u
        for c in this do
            h <- h ^^^ uint32(c)
            h <- h * 16777619u
        h
        
    let BOW = "<"
    let EOW = ">"
    let MAX_VOCAB_SIZE = 30000000
    let bucket = if wordNgrams <= 1uy && maxn = 0uy then 0 else bucket 
    let mutable size_ = 0
    let mutable nwords_ = 0
    let mutable nlabels_ = 0
    let mutable ntokens_ = 0
    let words_ = ResizeArray<Entry>()
    let mutable pdiscard_ : ResizeArray<float32> = null
    let word2int_ = ResizeArray<int>(Array.create MAX_VOCAB_SIZE -1)
    do
        assert(maxn = 0uy || wordNgrams <= 1uy)//currently chargrams and wordgrams are not supported at the same time
        
    member private x.Find(w : String) =
        let mutable h = int(getHash(w) % uint32(MAX_VOCAB_SIZE))
        while word2int_.[h] <> -1 && not(words_.[word2int_.[h]].word = w) do
            h <- (h + 1) % MAX_VOCAB_SIZE
        h

    member x.Add(w : String) =
        let h = x.Find(w)
        ntokens_ <- ntokens_ + 1
        if word2int_.[h] = -1 
        then
            let et : EntryType = if w.StartsWith(label) then EntryType.Label else EntryType.Word
            let e = Entry(w, 1L, et, ResizeArray<_>(), false)
            
            words_.Add(e)
            word2int_.[h] <- size_
            size_ <- size_ + 1
        else
            let mutable x = words_.[word2int_.[h]]
            x.count <- x.count + 1L
            words_.[word2int_.[h]] <- x

    member x.GetWords() =
        seq{
            for i = 0 to nwords_ do
                yield words_.[i].word
        }
    member x.Nwords() = nwords_

    member x.Nlabels() = nlabels_

    member x.Ntokens() = ntokens_

    member x.GetNgrams(i : int) =
        assert(i >= 0)
        assert(i < nwords_)
        words_.[i].subwords

    member x.GetNgrams(word : String) =
        let mutable ngrams = ResizeArray<int>()
        let i = x.GetId(word)
        if i >= 0 then ngrams <- words_.[i].subwords
        else x.ComputeNgrams(BOW + word + EOW, ngrams)
        ngrams

    member x.Discard(id : int, rand : float32) =
        assert(id >= 0)
        //assert(id < nwords_) //this is incorrect in source cpp needs to be size check thresold fn
        useDiscard && rand > pdiscard_.[id]

    member x.GetId(w : String) =
        let h = x.Find(w)
        word2int_.[h]

    member private x.GetType(id : int) =
        assert(id >= 0)
        assert(id < size_)
        words_.[id].etype

    member x.GetWord(id : int) =
        assert(id >= 0)
        assert(id < size_)
        words_.[id].word

    member x.ComputeNgrams(word : String, ngrams : ResizeArray<int>) =
        for i = 0 to word.Length - 1 do
        let ngram = ResizeArray<char>()
        let mutable j = i
        let mutable n = 1uy
        while j < word.Length && n <= maxn do
            ngram.Add(word.[j])
            j <- j + 1
            if n >= minn
            then let h : int = int(getHash(ngram) % uint32(bucket)) 
                 ngrams.Add(nwords_ + h)
            n <- n + 1uy

    member x.InitNgrams () =
        for i = 0 to size_ - 1 do
            let word = BOW + words_.[i].word + EOW
            words_.[i].subwords.Add(i);
            x.ComputeNgrams(word, words_.[i].subwords)

    member x.ReadFromFile(inp : seq<String>) =
        let mutable minThreshold = 1L
        for word in inp do
            x.Add(word)
            if ntokens_ % 1000000 = 0 && verbose
            then printf "\rRead %d M words" (ntokens_  / 1000000)
            if size_ > (MAX_VOCAB_SIZE / 4 * 3)
            then x.Threshold(minThreshold)
                 minThreshold <- minThreshold + 1L
        printfn "\rRead %d M words" (ntokens_  / 1000000)
        x.Threshold(int64(minCount))
        x.InitTableDiscard()
        x.InitNgrams ()
        printfn "Number of words:  %d" nwords_
        printfn "Number of labels: %d" nlabels_
        if size_ = 0
        then failwith "Empty vocabulary. Try a smaller -minCount value." 

    member x.Threshold(t : int64) =
        words_.Sort(fun e1 e2 -> if e1.etype <> e2.etype 
                                    then e1.etype.CompareTo(e2.etype)
                                    else -e1.count.CompareTo(e2.count))
        words_.RemoveAll(fun e -> e.etype = EntryType.Word && e.count < t) |> ignore
        //shrink
        if words_.Count < words_.Capacity
        then words_.Capacity <- words_.Count
        size_ <- 0
        nwords_ <- 0
        nlabels_ <- 0
        for i = 0 to MAX_VOCAB_SIZE - 1 do
            word2int_.[i] <- -1

        words_.ForEach(fun it ->  
            let h = x.Find(it.word)
            word2int_.[h] <- size_
            size_ <- size_ + 1
            if it.etype = EntryType.Word then nwords_ <- nwords_ + 1
            if it.etype = EntryType.Label then nlabels_ <- nlabels_ + 1
        )

    member x.InitTableDiscard() =
          
        pdiscard_ <- ResizeArray<float32>(size_)
        for i = 0 to size_ - 1 do
            let f = float32(words_.[i].count) / float32(ntokens_)
            pdiscard_.Add(sqrt(t / f) + t / f)

    member x.GetCounts(etype : EntryType) =
        let counts = ResizeArray<int64>()
        for i = 0 to words_.Count - 1 do
            if words_.[i].etype = etype then counts.Add(words_.[i].count)
        counts

    member x.AddWordNgrams(line : ResizeArray<int>) =
        if wordNgrams > 1uy 
        then let line_size = line.Count
             for i = 0 to line_size - 1 do
                let mutable h = uint64(line.[i])
                for j = i + 1 to min (line_size - 1) (i + int(wordNgrams)) do
                    h <- h * 116049371uL + uint64(line.[j])
                    line.Add(nwords_ + int(h % uint64(bucket)))

    member x.vectorize(rng : Mcg31m1) (line : string[]) =
        let words = ResizeArray<int>()
        let labels = ResizeArray<int>()
        let ids = line |> Seq.map x.GetId
                        |> Seq.where (fun wid -> wid >= 0)
        for wid in ids do
            let etype = x.GetType(wid)
            if etype = EntryType.Word && not (x.Discard(wid, rng.Sample()))
            then words.Add(wid)
            if etype = EntryType.Label 
            then labels.Add(wid - nwords_)
        words, labels


    member x.GetLabel(lid : int) =
        assert(lid >= 0)
        assert(lid < nlabels_)
        words_.[lid + nwords_].word

    member x.Save(out : System.IO.BinaryWriter) =
        out.Write(size_)
        out.Write(nwords_)
        out.Write(nlabels_)
        out.Write(ntokens_)
        for i = 0 to size_ - 1 do
            let e = words_.[i]
            let bytes = System.Text.Encoding.UTF8.GetBytes(e.word)
            out.Write(bytes)
            out.Write(0uy)
            out.Write(e.count)
            out.Write(byte(e.etype))

    member x.Load(inp : System.IO.BinaryReader) =
        words_.Clear()
        for i = 0 to MAX_VOCAB_SIZE - 1 do
            word2int_.[i] <- -1
        size_ <- inp.ReadInt32()
        nwords_ <- inp.ReadInt32()
        nlabels_ <- inp.ReadInt32()
        ntokens_ <- inp.ReadInt32()
        for i = 0 to size_ - 1 do
            let utf8word = ResizeArray<byte>()
            let mutable c = inp.ReadByte()
            while c <> 0uy do
                utf8word.Add(c)
                c <- inp.ReadByte()
            let word = System.Text.Encoding.UTF8.GetString(utf8word.ToArray())
            let count = inp.ReadInt64()
            let etype : EntryType = LanguagePrimitives.EnumOfValue <| inp.ReadByte()
            words_.Add(Entry(word, count, etype, ResizeArray(), false))
            word2int_.[x.Find(word)] <- i
        x.InitTableDiscard()
        x.InitNgrams ()


