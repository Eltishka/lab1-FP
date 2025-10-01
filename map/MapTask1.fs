module MapTask1

let generateProducts (num: string) =
    [ 0 .. (num.Length - 13) ]
    |> Seq.map (fun offset ->
        (1L, [ 0..12 ])
        ||> Seq.fold (fun acc i -> acc * int64 (int num[offset + i] - int '0')))

let findMax (num: string) =
    generateProducts num |> Seq.fold (fun res x -> max res x) 0L
