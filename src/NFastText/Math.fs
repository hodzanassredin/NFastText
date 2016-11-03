namespace NFastText

open System.Runtime.CompilerServices
type Vector = float32[]
type Matrix  = {
    data : float32[][]
    m : int
    n : int
}

[<Extension>]
type VectorExts() =
    [<Extension>]
    static member Zero(this: Vector) = 
        Array.fill this 0 this.Length 0.0f
    [<Extension>]
    static member M(this: Vector) = this.Length
    [<Extension>]
    static member Mul(this: Vector, a : float32) = 
        for i = 0 to this.Length - 1 do
            this.[i] <- this.[i] * a
    [<Extension>]
    static member AddRow(this: Vector, A : Matrix, i : int) =  
        assert (i >= 0)
        assert (i < A.m)
        assert (this.Length = A.n)
        let m = A.data.[i]
        for j in 0..(A.n - 1) do
            this.[j] <- this.[j] + m.[j]
    [<Extension>]
    static member AddRow(this: Vector, A : Matrix, i : int, a : float32) =  
        assert (i >= 0)
        assert (i < A.m)
        assert (this.Length = A.n)
        let m = A.data.[i]
        for j in 0..(A.n - 1) do
            this.[j] <- this.[j] + a * m.[j]
    [<Extension>]
    static member Mul(this: Vector, A : Matrix, vec : Vector) =
        assert(A.m = this.Length)
        assert(A.n = vec.Length)
        for i in 0..this.Length - 1 do
        this.[i] <- 0.0f
        let m = A.data.[i]
        for j in 0..(A.n - 1) do
            this.[i] <- this.[i] + m.[j] * vec.[j]
    [<Extension>]
    static member Argmax(this: Vector) : int =
        let mutable max = this.[0]
        let mutable argmax = 0
        for i in 0..this.Length - 1 do
            if this.[i] > max then
                max <- this.[i]
                argmax <- i
        argmax

    [<Extension>]
    static member WriteTo(this: Vector, s:System.IO.BinaryWriter) = 
        Array.iter (fun (v:float32) -> s.Write(v);s.Write(Utils.spaceCode)) this

[<Extension>]
type MatrixExts() =
    [<Extension>]
    static member Zero(this: Matrix) = 
        for i = 0 to this.m - 1 do
            for j = 0 to this.n - 1 do
                this.data.[i].[j] <- 0.0f

    [<Extension>]
    static member Uniform(this: Matrix, a : float32) = 
        let rng = Mcg31m1(1)
        for i = 0 to this.m - 1 do
            for j = 0 to this.n - 1 do
                this.data.[i].[j] <- rng.ConUniformSample(-a,a)
    [<Extension>]
    static member AddRow(this: Matrix, vec : Vector, i, a) =
        assert(i >= 0)
        assert(i < this.m)
        assert(vec.Length = this.n)
        let m = this.data.[i]
        for j = 0 to vec.Length - 1 do
        m.[j] <- m.[j] + a * vec.[j]

    [<Extension>]
    static member DotRow(this: Matrix, vec : Vector, i) =
        assert(i >= 0)
        assert(i < this.m)
        assert(vec.Length = this.n)
        let mutable d = 0.0f
        let m = this.data.[i]
        for j = 0 to vec.Length - 1 do
            d <- d + m.[j] * vec.[j]
        assert(System.Single.IsNaN(d) |> not)
        d

module Math =
    
    let createVector(m) : Vector = Array.zeroCreate m

    let create(m,n) : Matrix = 
        let mat = Array.zeroCreate m
        for i = 0 to m - 1 do
            mat.[i] <- Array.zeroCreate n
        {
            data = mat
            m = m
            n = n
        }
    let createNull() = create(0,0)

    let load(inp : System.IO.BinaryReader) : Matrix =
          let m = int(inp.ReadInt64())
          let n = int(inp.ReadInt64())
          let r = create(m,n)
          for i = 0 to m - 1 do
            for j = 0 to n - 1 do 
                r.data.[i].[j] <- inp.ReadSingle()
          r

    let save(this: Matrix, out : System.IO.BinaryWriter) =
          out.Write(int64(this.m))//todo int int64
          out.Write(int64(this.n))
          for i = 0 to this.m - 1 do
            for j = 0 to this.n - 1 do 
                out.Write(this.data.[i].[j])
    