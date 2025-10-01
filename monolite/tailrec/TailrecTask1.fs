module TailrecTask1

let thirteenProduct num offset =
    let rec solve (num: string) (offset: int) (i: int) (ans: int64) : int64 =
        if i = 13 then
            ans
        else
            solve num offset (i + 1) (ans * int64 (int num[offset + i] - int '0'))

    solve num offset 0 1L

let findMaxThirteenProduct (num: string) =
    let rec solve (num: string) (i: int) (res: int64) =
        if i = num.Length - 13 then
            res
        else
            solve num (i + 1) (max res (thirteenProduct num i))

    solve num 0 0L
