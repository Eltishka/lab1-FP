module InfiniteTask1

let findMax (num: string) =
    Seq.initInfinite (fun offset ->
        (1L, [ 0..12 ])
        ||> Seq.fold (fun acc i -> acc * int64 (int num[offset + i] - int '0')))
    |> Seq.take (num.Length - 13)
    |> Seq.reduce (fun res x -> max res x)
