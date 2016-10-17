namespace NFastText

[<AutoOpen>]
module ByteString =
    open System
    open System.Runtime.CompilerServices
    let fromString(s : string) = s
    
    [<Extension>]
    type ArrayExts () =
        [<Extension>]
        static member Clear(_ : String) = ()
        [<Extension>]
        static member Copy(this : String) = this
        [<Extension>]
        static member StartsWith(this : String, sub : String) = 
            this.StartsWith(sub)

        [<Extension>]
        static member ToStr(this : String) = this

        [<Extension>]
        static member Hash(this : System.Collections.Generic.IEnumerable<char>) = 
            let mutable h = 2166136261u
            for c in this do
                h <- h ^^^ uint32(c)
                h <- h * 16777619u
            h
        [<Extension>]
        static member Eq (x : String, y : String) = 
            x = y

        [<Extension>]
        static member Wrap (v : String, pref : String, suff : String) =
            pref + v + suff