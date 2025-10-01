module RecTask2

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

let sumOfNotPresentedAsTwoAbundant last =
    let rec solve from last =
        let toAdd = if isPresentedAsTwoAbundant from then 0 else from
        if from = last then toAdd else toAdd + solve (from + 1) last

    let q1 = solve 1 (last / 3)
    let q2 = solve (last / 3 + 1) (last / 3 * 2)
    let q3 = solve (last / 3 * 2 + 1) last

    q1 + q2 + q3
