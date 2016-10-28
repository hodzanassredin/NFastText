namespace NFastText

module FileReader = 
    open System.IO
    let splitters = [|' '; '\t'; '\n'; '\v'; '\f'; '\r'|]

    let streamToLines (s:Stream) =
        let r = new StreamReader(s)
        let mutable line = r.ReadLine()
        seq {
            while line <> null do
                yield line.Split(splitters)
                line <- r.ReadLine()
        }
    
    let infinite (mapper: Stream -> seq<_>) (s:Stream) =
        seq{
            while true do
                s.Position <- 0L
                yield! mapper s
        }

    let streamToWords (s:Stream) =
        let r = new StreamReader(s)
        let buffer = Array.zeroCreate 80
        let mutable readedChars = buffer.Length
        let readedLine = System.Text.StringBuilder(buffer.Length)
        seq{
            while readedChars = buffer.Length do
                readedChars <- r.ReadBlock(buffer, 0, buffer.Length)
                let rec loop i =
                    if i < 0 then -1
                    else if Array.contains buffer.[i] splitters
                         then i
                         else loop (i - 1)
                let lastSplit = loop (readedChars - 1)
                if lastSplit = -1 
                then readedLine.Append(buffer, 0, readedChars) |> ignore
                else
                    readedLine.Append(buffer, 0, lastSplit) |> ignore

                    let str = readedLine.ToString()
                    readedLine.Clear() |> ignore
                    if (lastSplit + 1) < readedChars
                    then 
                        readedLine.Append(buffer, lastSplit + 1, readedChars - lastSplit - 1) |> ignore

                    for word in str.Split(splitters) do
                        if word.Length > 0 then yield word
                              
            if readedLine.Length > 0 then yield readedLine.ToString()
        }
    

    let streamToWordsChunks maxWordsInLine = streamToWords >> Seq.chunkBySize maxWordsInLine


