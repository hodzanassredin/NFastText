namespace NFastText
[<AutoOpen>]
module BaseTypes = 
    open System.Runtime.CompilerServices
    open System.IO

    let split length (xs: seq<'T>) =
        let rec loop xs =
            seq{
                yield Seq.truncate length xs 
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            }
        loop xs

    type BinaryReader (s : System.IO.Stream) = 
        let r = new StreamReader(s)
                           
        member x.Length = s.Length
        member x.Close() = s.Close()

        interface System.IDisposable with 
            member this.Dispose() = s.Dispose()



        member x.readWords() = 
          seq{
            let mutable line = r.ReadLine()
            while line <> null do
                yield! line.Split([|' '; '\t'; '\n'; '\v'; '\f'; '\r'|])
                line <- r.ReadLine()
          }
        member x.readLines(max_line_size, fromStartOnEof) =
             seq{
                let mutable line = r.ReadLine()
                while line <> null do
                    let lnWords = line.Split([|' '; '\t'; '\n'; '\v'; '\f'; '\r'|])
                    for chunk in split max_line_size lnWords do
                        yield chunk 
                    line <- r.ReadLine()

                if fromStartOnEof 
                then s.Position <- 0L
                     yield! x.readLines(max_line_size, fromStartOnEof)
              }


    let binaryWriter(filename) = new System.IO.BinaryWriter(System.IO.File.Open(filename, System.IO.FileMode.Create))
    let binaryReader(filename) = 
            let s = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
            new System.IO.BinaryReader(s)








