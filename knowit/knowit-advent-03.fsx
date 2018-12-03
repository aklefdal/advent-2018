let isPrime n = 
    let rec nonDivisible by =
        if by = 1L then true        // Return 'true' if we reached the end
        elif n%by = 0L then false   // Return 'false' if there is a divisor
        else nonDivisible (by - 1L) // Otherwise continue looping

    n > 1L && nonDivisible (n/2L)

let powd (exp:int) (f:int64) = List.replicate exp f |> List.fold (*) 1L

let max = 4294967296L
for threes in [0 .. 24] do
    let twos = 24 - threes
    let product = (2L |> powd twos) * (3L |> powd threes)
    printfn "%i twos, %i threes, product %i, Too big? %b" twos threes product (product > max)


let min = 2L |> powd 24
let largestPossibleFactor = max / (2L |> powd 23)

let primes = seq { 0L .. largestPossibleFactor } |> Seq.filter isPrime |> Seq.toArray

let shouldQuit (num:int64) primeNum (acc:int64 list) =
    let remainingFactors = 24 - acc.Length
    if remainingFactors <= 0 then true
    elif primeNum >= primes.Length - 1 then true
    else false

let rec getFactor num primeNum acc = 
    let proposed = primes.[primeNum]
    if proposed = num then
        proposed::acc |> List.length |> ((=) 13)
    elif num % proposed = 0L then 
        getFactor (num/proposed) primeNum (proposed::acc)
    elif shouldQuit num primeNum acc then 
        false
    else
        getFactor num (primeNum + 1) acc

let factorise n = 
    let elevenTwos = 2L |> powd 11
    match n % elevenTwos with
    | 0L -> getFactor (n / elevenTwos) 0 []
    | _ -> false

62914560L |> factorise
10240000L |> factorise
32089034L |> factorise
16777216L |> factorise
55023912L |> factorise
25165824L |> factorise

[ 10240000L
  32089034L
  16777216L
  55023912L
  25165824L ]
|> List.map factorise
|> List.filter id
|> List.length

seq { min .. 4294967296L }
//seq { min .. 55023912L }
|> Seq.map factorise
|> Seq.filter id
|> Seq.length
