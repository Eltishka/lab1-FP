module CycleTask1

let findMax (num: string) =
    let mutable maxProduct = 0L
    let lastIndex = num.Length - 13

    for offset in 0..lastIndex do
        let mutable product = 1L

        for i in 0..12 do
            product <- product * int64 (int num[offset + i] - int '0')

        if product > maxProduct then
            maxProduct <- product

    maxProduct
