namespace NFastText
module Utils = 
    let spaceCode = 0x20uy

    let SIGMOID_TABLE_SIZE = 512
    let SIGMOID_TABLE_SIZEf = float32(SIGMOID_TABLE_SIZE)
    let MAX_SIGMOID = 8
    let MAX_SIGMOIDf = float32(MAX_SIGMOID)
    let LOG_TABLE_SIZE = 512
    let LOG_TABLE_SIZEf = float32(LOG_TABLE_SIZE)

    let t_sigmoid : float32[] = Array.zeroCreate (SIGMOID_TABLE_SIZE + 1)
    
    for i in 0..SIGMOID_TABLE_SIZE do
        let x = float32(i * 2 * MAX_SIGMOID) / SIGMOID_TABLE_SIZEf - MAX_SIGMOIDf
        t_sigmoid.[i] <- 1.0f / (1.0f + exp(-x))

    let t_log = Array.zeroCreate (LOG_TABLE_SIZE + 1)
    for i in 0..LOG_TABLE_SIZE do
        let x = (float32(i) + 1e-5f) / LOG_TABLE_SIZEf
        t_log.[i] <- log(x)

    let log(x : float32) : float32 =
        if x > 1.0f then 0.0f
        else let i = int(x * LOG_TABLE_SIZEf)
             t_log.[i]

    let sigmoid(x : float32) : float32 =
        if x < float32(-MAX_SIGMOID) then 0.0f
        else if x > float32(MAX_SIGMOID) then 1.0f
        else let i = int((x + MAX_SIGMOIDf) * SIGMOID_TABLE_SIZEf / MAX_SIGMOIDf / 2.f)
             assert(i >= 0 && i <= SIGMOID_TABLE_SIZE)
             t_sigmoid.[i] //todo

