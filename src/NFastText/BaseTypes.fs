namespace NFastText
[<AutoOpen>]
module BaseTypes = 
    open System.Runtime.CompilerServices
    open System.IO
    [<Extension>]
    type Exts() =
        [<Extension>]
        static member Resize(this: ResizeArray<'a>, size : int) = 
            if this.Count > size
            then this.RemoveRange(size, this.Count - size)
            else this.AddRange(System.Linq.Enumerable.Repeat(Unchecked.defaultof<'a>, size - this.Count))
        [<Extension>]
        static member ShrinkToFit(this: ResizeArray<'a>) = 
            if this.Count < this.Capacity
            then this.Capacity <- this.Count

    type BinaryReader (s : System.IO.Stream) = 
        let EOS = ByteString.fromString("</s>")
        
        let buff_size = 10000000
        let buff : byte[] = Array.zeroCreate buff_size 
        let len = s.Length
        let mutable pos = s.Position
        let mutable buff_pos = pos
        do
            s.Read(buff, 0, buff_size) |>ignore
        new(filename) = let stream = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
                        new BinaryReader(stream)

        member private x.ReadByte() = 
                              if pos >= len 
                              then raise <| System.IO.EndOfStreamException()
                              let i = int(pos - buff_pos)
                              if i < 0 //unget handle
                              then pos <- 0L
                                   0x0auy // \n
                              elif i < buff_size 
                              then pos <- pos + 1L
                                   buff.[i]
                              else s.Read(buff, 0, buff_size) |>ignore
                                   buff_pos <- pos
                                   pos <- pos + 1L
                                   buff.[0]

        member x.EOF() = pos >= len
        member x.NotEOF() = pos < len
        member x.MoveAbs(l) = if l >= buff_pos && l < (buff_pos + int64(buff_size))
                              then pos <- l
                              else s.Position <- l
                                   pos <- l
                                   buff_pos <- l
                                   s.Read(buff, 0, buff_size) |>ignore

        member x.Unget() = pos <- pos - 1L
                           
        member x.Length = len
        member x.Close() = s.Close()

        interface System.IDisposable with 
            member this.Dispose() = s.Dispose()

        //        ' '	(0x20)	space (SPC)
        //        '\t'	(0x09)	horizontal tab (TAB)
        //        '\n'	(0x0a)	newline (LF)
        //        '\v'	(0x0b)	vertical tab (VT)
        //        '\f'	(0x0c)	feed (FF)
        //        '\r'	(0x0d)	carriage return (CR)

        static member isspace(c : byte) = 
          c = 0x20uy || c = 0x09uy || c = 0x0auy || c = 0x0buy || c = 0x0cuy || c = 0x0duy

        member x.readWordInt(inp : BinaryReader, word : String) = 
            if inp.EOF() 
            then word.Count > 0
            else
                let c = inp.ReadByte()
                if BinaryReader.isspace(c) || c = 0uy 
                then
                    if word.Count = 0
                    then
                        if c = 0x0auy // \n
                        then word.AddRange(EOS)
                             true
                        else x.readWordInt(inp, word)
                    else
                        if c = 0x0auy // \n
                        then inp.Unget()
                        true
                else word.Add(c)
                     x.readWordInt(inp, word)

        member x.readWords() = 
          let word = String()
          seq{
            while x.readWordInt(x, word) do
                yield word.Copy()
                word.Clear()
          }
        member x.readLines(max_line_size, fromStartOnEof) =
             seq{
                    let en = x.readWords().GetEnumerator()
                    let mutable notEof = en.MoveNext()
                    while notEof do
                        yield seq{
                            let mutable wordsCount = 0
                            while notEof && (en.Current.Eq EOS |> not) && wordsCount < max_line_size do
                                yield en.Current
                                notEof <- en.MoveNext()
                                wordsCount <- wordsCount + 1
                        }
                        if en.Current.Eq EOS then notEof <- en.MoveNext()
                    if fromStartOnEof 
                    then x.MoveAbs(0L) 
                         yield! x.readLines(max_line_size, fromStartOnEof)
              }


    let binaryWriter(filename) = new System.IO.BinaryWriter(System.IO.File.Open(filename, System.IO.FileMode.Create))
    let binaryReader(filename) = 
            let s = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
            new System.IO.BinaryReader(s)








