namespace NFastText
open System.IO

module Seq =
    let chunkBySize chunkSize (source : seq<_>) =
            seq { use e = source.GetEnumerator()
                  let nextChunk() =
                      let res = Array.zeroCreate chunkSize
                      res.[0] <- e.Current
                      let i = ref 1
                      while !i < chunkSize && e.MoveNext() do
                          res.[!i] <- e.Current
                          i := !i + 1
                      if !i = chunkSize then
                          res
                      else
                          Array.sub res 0 !i 
                  while e.MoveNext() do
                      yield nextChunk() }

module Array =
    let contains e (array:'T[]) =
            let mutable state = false
            let mutable i = 0
            while not state && i < array.Length do
                state <- e = array.[i]
                i <- i + 1
            state 

type Input =
    | Stream of Stream
    | FilePath of string

    member self.ToStream() =
        match self with
            | Stream s -> s
            | FilePath path -> upcast System.IO.File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read)

module FileReader = 
    let splitters = [|' '; '\t'; '\n'; '\v'; '\f'; '\r'|]

    let streamToLines (s:Input) =
        let r = new StreamReader(s.ToStream())
        let mutable line = r.ReadLine()
        seq {
            while line <> null  do
                yield line.Split(splitters)
                line <- r.ReadLine()
        }
    
    let infinite (mapper: Input -> seq<_>) (s:Input) =
        let s = s.ToStream()
        seq{
            while true do
                s.Position <- 0L
                yield! mapper <| Stream(s)
        }

    let streamToWords (s:Input) =
        let r = new StreamReader(s.ToStream())
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
    

    let streamToWordsChunks x = x |> streamToWords |> Seq.chunkBySize 1024

    let split count input : Input list = 
        match input with 
            | Stream _ -> [input]
            | FilePath _ -> [0..count - 1] 
                            |> List.map (fun i -> i, input.ToStream())
                            |> List.map (fun (i,s) -> s.Position <- s.Length / int64(count) * int64(i)
                                                      Stream s)



