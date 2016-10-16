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
        let r = new StreamReader(s)
        let buff_size = 10000000
        let buff : char[] = Array.zeroCreate buff_size 

        let mutable pos = s.Position
        let mutable buff_pos = pos
        let mutable lasBlockSize = 0
        let read_next_block() =
            lasBlockSize <- r.Read(buff, 0, buff_size) 
        do
            read_next_block()
        new(filename) = let stream = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
                        new BinaryReader(stream)

        member private x.ReadByte() : char = 
                              if x.EOF()
                              then raise <| System.IO.EndOfStreamException()
                              let i = int(pos - buff_pos)
                              if i < 0 //unget handle
                              then pos <- 0L
                                   '\n'
                              elif i < buff_size 
                              then pos <- pos + 1L
                                   buff.[i]
                              else read_next_block()
                                   buff_pos <- pos
                                   pos <- pos + 1L
                                   buff.[0]

        member x.EOF() = lasBlockSize < int(pos - buff_pos)
        member x.NotEOF() = not <| x.EOF()
        member x.MoveAbs(l) = s.Position <- l
                              pos <- l
                              buff_pos <- l
                              read_next_block()

        member x.Unget() = pos <- pos - 1L
                           
        member x.Length = s.Length
        member x.Close() = s.Close()

        interface System.IDisposable with 
            member this.Dispose() = s.Dispose()

        //        ' '	(0x20)	space (SPC)
        //        '\t'	(0x09)	horizontal tab (TAB)
        //        '\n'	(0x0a)	newline (LF)
        //        '\v'	(0x0b)	vertical tab (VT)
        //        '\f'	(0x0c)	feed (FF)
        //        '\r'	(0x0d)	carriage return (CR)

        static member isspace(c : char) = 
          c = ' ' || c = '\t' || c = '\n' || c = '\v' || c = '\f' || c = '\r'

        member x.readWordInt(inp : BinaryReader, word : String) = 
            if inp.EOF() 
            then word.Count > 0
            else
                let c = inp.ReadByte()
                if BinaryReader.isspace(c) || int(c) = 0 
                then
                    if word.Count = 0
                    then
                        if c = '\n' // \n
                        then word.AddRange(EOS)
                             true
                        else x.readWordInt(inp, word)
                    else
                        if c = '\n' // \n
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
                        if en.Current <> null && en.Current.Eq EOS then notEof <- en.MoveNext()
                    if fromStartOnEof 
                    then x.MoveAbs(0L) 
                         yield! x.readLines(max_line_size, fromStartOnEof)
              }


    let binaryWriter(filename) = new System.IO.BinaryWriter(System.IO.File.Open(filename, System.IO.FileMode.Create))
    let binaryReader(filename) = 
            let s = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
            new System.IO.BinaryReader(s)








