
let sumOfDivisors (n: int) =
  let upper = int (sqrt (float n))
  let mutable acc = 0
  for d in 1 .. upper do
    if n % d = 0 then
      let other = n / d
      if d = other || other = n then acc <- acc + d
      else acc <- acc + d + other
  acc

let isPresentedAsTwoAbundant (num: int) =
    let mutable result = false
    let mutable firstNum = 12
    while firstNum < num && not result do
        if sumOfDivisors firstNum > firstNum && sumOfDivisors (num - firstNum) > num - firstNum then
            result <- true
        else
            firstNum <- firstNum + 1
    result

let sumOfNotPresentedAsTwoAbundant limit =
    let mutable sum = 0
    for x in 1 .. limit do
        if not (isPresentedAsTwoAbundant x) then
            sum <- sum + x
    sum

sumOfNotPresentedAsTwoAbundant 28123
