namespace NFastText

[<AutoOpen>]
module ByteString =

    type String = ResizeArray<char>
    open System.Runtime.CompilerServices
    let fromString(s : string) = String(s.ToCharArray())
    
    [<Extension>]
    type ArrayExts () =
        [<Extension>]
        static member Clear(this : String) = this.RemoveRange(0, this.Count)
        [<Extension>]
        static member Copy(this : String) = ResizeArray<char>(this.ToArray())
        [<Extension>]
        static member StartsWith(this : String, sub : String) = 
            let mutable i = 0
            if sub.Count > this.Count 
            then false
            else while i < sub.Count && this.[i] = sub.[i] do
                    i <- i + 1
                 i = sub.Count

        [<Extension>]
        static member ToStr(this : String) = System.String.Join("", this)

        [<Extension>]
        static member Hash(this : String) = 
            let mutable h = 2166136261u
            for i = 0 to this.Count - 1 do
                h <- h ^^^ uint32(this.[i])
                h <- h * 16777619u
            h
        [<Extension>]
        static member Eq (x : String, y : String) =
            x.Count = y.Count && ArrayExts.StartsWith(x,y)

        [<Extension>]
        static member Wrap (v : String, pref : String, suff : String) =
            let sum = String(v.Count + pref.Count + suff.Count)
            sum.AddRange(pref)
            sum.AddRange(v)
            sum.AddRange(suff)
            sum