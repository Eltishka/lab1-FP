# Лабораторная работа 1

**Выполнил:** Кузнецов Кирилл Андреевич, P3330

## Описание задач
### Task1: Задача Project Euler №8

Найти наибольшее произведение 13-ти подряд идущих цифр в 1000-значном числе:
```
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
```

### Task2: Задача Project Euler №23

Найти сумму всех положительных чисел, которые не могу быть представленны, как два **abundant** числа, где числа **abudant** это те, у которых сумма делителей больше самого числа.

## Способы реализации задач:

1. Хвостовая рекурсия
2. Обычная рекурсия
3. Модульная реализация (генерация → фильтрация → свёртка)
4. Map
5. Циклы
6. Бесконечные последовательности

## Элементы реализации

### Хвостовая рекурсия

#### Task1
```F#
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
```
#### Task2
```F#
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
        if firstNum > num / 2 then
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
    let rec solve current acc =
        if current > last then
            acc
        else
            let toAdd = if isPresentedAsTwoAbundant current then 0 else current
            solve (current + 1) (acc + toAdd)

    solve 1 0
```
### Обычная рекурсия

#### Task1
```F#
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
```
#### Task2
```F#
let sumOfDivisors (n: int) = //Реализация не отличается от предыдущей

let isPresentedAsTwoAbundant (num: int) = //Реализация не отличается от предыдущей

let sumOfNotPresentedAsTwoAbundant last =
    let rec solve from last =
        let toAdd = if isPresentedAsTwoAbundant from then 0 else from
        if from = last then toAdd else toAdd + solve (from + 1) last

    //разделим задачу на 3, чтобы избежать StackOverflow
    let q1 = solve 1 (last / 3)
    let q2 = solve (last / 3 + 1) (last / 3 * 2)
    let q3 = solve (last / 3 * 2 + 1) last

    q1 + q2 + q3 
```
### Модульная реализация

#### Task1
```F#
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
```
#### Task2
```F#
let sumOfDivisors (n: int) = //Реализация не отличается от предыдущей

let isPresentedAsTwoAbundant (num: int) = //Реализация не отличается от предыдущей

let generateNums num = [ 1..num ]

let takeByCond num cond = generateNums num |> Seq.filter (cond)

let sumOfNotPresentedAsTwoAbundant num =
    takeByCond num (fun x -> not (isPresentedAsTwoAbundant x))
    |> Seq.reduce (fun res x -> res + x)
```
### Map

#### Task1
```F#
let generateProducts (num: string) =
    [ 0 .. (num.Length - 13) ]
    |> Seq.map (fun offset ->
        (1L, [ 0..12 ])
        ||> Seq.fold (fun acc i -> acc * int64 (int num[offset + i] - int '0')))

let findMax (num: string) =
    generateProducts num |> Seq.fold (fun res x -> max res x) 0L
```
#### Task2
```F#
let sumOfDivisors (n: int) = //Реализация не отличается от предыдущей

let isPresentedAsTwoAbundant (num: int) = //Реализация не отличается от предыдущей

let generateNums num = [ 1..num ]

let sumOfNotPresentedAsTwoAbundant num =
    generateNums num
    |> Seq.map (fun x -> if isPresentedAsTwoAbundant x then None else Some x)
    |> Seq.choose id
    |> Seq.sum
```
### Циклы

#### Task1
```F#
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
```
#### Task2
```F#
let sumOfDivisors (n: int) =
    let upper = int (sqrt (float n))
    let mutable acc = 0

    for d in 1..upper do
        if n % d = 0 then
            let other = n / d

            if d = other || other = n then
                acc <- acc + d
            else
                acc <- acc + d + other

    acc

let isPresentedAsTwoAbundant (num: int) =
    let mutable result = false
    let mutable firstNum = 12

    while firstNum < num && not result do
        if
            sumOfDivisors firstNum > firstNum
            && sumOfDivisors (num - firstNum) > num - firstNum
        then
            result <- true
        else
            firstNum <- firstNum + 1

    result

let sumOfNotPresentedAsTwoAbundant limit =
    let mutable sum = 0

    for x in 1..limit do
        if not (isPresentedAsTwoAbundant x) then
            sum <- sum + x

    sum
```
### Бесконечные последовательности

#### Task1
```F#
let findMax (num: string) =
    Seq.initInfinite (fun offset ->
        (1L, [ 0..12 ])
        ||> Seq.fold (fun acc i -> acc * int64 (int num[offset + i] - int '0')))
    |> Seq.take (num.Length - 13)
    |> Seq.reduce (fun res x -> max res x)
```
#### Task2
```F#
let sumOfDivisors (n: int) = //Реализация не отличается от реализации в хвостовой

let isPresentedAsTwoAbundant (num: int) = //Реализация не отличается от реализации в хвостовой

let generateNums = Seq.initInfinite (fun i -> i + 1)

let takeByCond limit cond =
    generateNums |> Seq.take limit |> Seq.filter cond

let sumOfNotPresentedAsTwoAbundant limit =
    takeByCond limit (fun x -> not (isPresentedAsTwoAbundant x)) |> Seq.sum
```
### Реализация на python

#### Task1
```python
def largest_product_in_series(digits):
    max_product = 0
    for i in range(len(digits) - 12):
        product = 1
        for j in range(13):
            product *= int(digits[i + j])
        max_product = max(max_product, product)
    
    return max_product
```
#### Task2
```python
def get_sum_of_divisors(n):
    s = 1
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            s += i
            if i != n // i:
                s += n // i
    return s

def is_abundant(n):
    return get_sum_of_divisors(n) > n

def sum_of_not_presented_as_two_abundant(limit):
    abundant_numbers = [i for i in range(1, limit + 1) if is_abundant(i)]

    can_be_written = set()
    for i in range(len(abundant_numbers)):
        for j in range(i, len(abundant_numbers)):
            total = abundant_numbers[i] + abundant_numbers[j]
            if total <= limit:
                can_be_written.add(total)
    
    cannot_be_written = [i for i in range(1, limit + 1) if i not in can_be_written]
    
    return sum(cannot_be_written)
```
