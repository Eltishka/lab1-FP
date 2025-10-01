module ModuleTask1

let generateProducts (num: string) =
    let thirteenProduct num offset =
        let rec solve (num: string) (offset: int) (i: int) (ans: int64) : int64 =
            if i = 13 then
                ans
            else
                solve num offset (i + 1) (ans * int64 (int num[offset + i] - int '0'))

        solve num offset 0 1L

    seq {
        for i in 0 .. (num.Length - 13) do
            yield thirteenProduct num i
    }

let filterZeros (num: string) =
    generateProducts num |> Seq.filter (fun d -> d <> 0L)

let findMax (num: string) =
    filterZeros num |> Seq.fold (fun res x -> max res x) 0L
