namespace NFastText
[<AutoOpen>]
module Heap =
    open System.Collections.Generic
    type MinHeap(array : ResizeArray<KeyValuePair<float32,int>>) = 
        new(cap : int) = MinHeap(new ResizeArray<KeyValuePair<float32,int>>(cap))
        new() = MinHeap(4)

        member x.Add(v) = array.Add(v)
                          x.MakeHeap()
        member x.Reserve(c) = array.Capacity <- c
        member x.MakeHeap() = 
            let mutable c = array.Count - 1;
            let mutable  parent = (c - 1) >>> 1
            while c > 0 && array.[c].Key.CompareTo(array.[parent].Key) < 0 do
                let tmp = array.[c];
                array.[c] <- array.[parent]
                array.[parent] <- tmp
                c <- parent
                parent <- (c - 1) >>> 1

        static member cycle(c, array : ResizeArray<KeyValuePair<float32,int>>) = 
                if c < array.Count
                then    let mutable min = c
                        if 2 * c + 1 < array.Count && array.[2 * c + 1].Key < array.[min].Key
                        then min <- 2 * c + 1
                        if 2 * c + 2 < array.Count && array.[2 * c + 2].Key < array.[min].Key
                        then min <- 2 * c + 2
                        if min <> c 
                        then let tmp = array.[c]
                             array.[c] <- array.[min]
                             array.[min] <- tmp
                             MinHeap.cycle(min, array)
            

        member x.RemoveBack() = 
            array.RemoveAt(0)
            MinHeap.cycle(0, array)

        member x.Front() = array.[0]

        member x.Count = array.Count

        member this.Item with get(index) = array.[index]