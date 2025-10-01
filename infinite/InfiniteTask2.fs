module InfiniteTask2

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

let generateNums = Seq.initInfinite (fun i -> i + 1)

let takeByCond limit cond =
    generateNums |> Seq.take limit |> Seq.filter cond

let sumOfNotPresentedAsTwoAbundant limit =
    takeByCond limit (fun x -> not (isPresentedAsTwoAbundant x)) |> Seq.sum
