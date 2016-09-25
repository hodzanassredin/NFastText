namespace NFastText 
module Random =
    type Mcg31m1(seedInit) =
        let _modulus : uint64 = 2147483647UL
        let _multiplier : uint64 = 1132489760UL
        let _reciprocal : float32 = 1.0f / float32(_modulus)
        let maxIntf = float32(System.Int32.MaxValue)

        let seed = if seedInit = 0 then 1 else seedInit
        let mutable _xn : uint64 = uint64(seed) % _modulus

        new() = Mcg31m1(int(System.DateTime.Now.Ticks))

        member x.Next() = int(x.Sample() * maxIntf)

        member x.Sample() : float32 = 
            let ret = float32(_xn) * _reciprocal
            _xn <- (_xn * _multiplier) % _modulus 
            ret

        member x.ConUniformSample(lower : float32, upper : float32) = 
            lower + (x.Sample() * (upper - lower))

        member x.DiscrUniformSample(lower : int, upper : int) =
            (x.Next() % (upper - lower + 1)) + lower

