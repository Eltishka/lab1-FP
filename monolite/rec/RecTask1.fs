module RecTask1

let thirteenProduct (num: string) (offset: int) : int64 =
    let rec solve (num: string) (offset: int) (i: int) : int64 =
        if i = 13 then
            1L
        else
            int64 (int num[offset + i] - int '0') * solve num offset (i + 1)

    solve num offset 0

let findMaxThirteenProduct (num: string) =
    let rec solve (i: int) =
        if i > num.Length - 13 then
            0L
        else
            max (solve (i + 1)) (thirteenProduct num i)

    solve 0
