module ModuleTask2

let sumOfDivisors (n: int) =
    let upper = int (sqrt (float n))

    [ 1..upper ]
    |> Seq.fold
        (fun acc d ->
            if n % d = 0 then
                let other = n / d
                if d = other || other = n then acc + d else acc + d + other
            else
                acc)
        0

let isPresentedAsTwoAbundant (num: int) =
    let rec solve firstNum =
        if firstNum >= num then
            false
        else if
            sumOfDivisors firstNum > firstNum
            && sumOfDivisors (num - firstNum) > num - firstNum
        then
            true
        else
            solve (firstNum + 1)

    solve 12

let generateNums num = [ 1..num ]

let takeByCond num cond = generateNums num |> Seq.filter (cond)

let sumOfNotPresentedAsTwoAbundant num =
    takeByCond num (fun x -> not (isPresentedAsTwoAbundant x))
    |> Seq.reduce (fun res x -> res + x)
