open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let sum01a = 
    Path.Combine(__SOURCE_DIRECTORY__, "AoC-01-input.txt")
    |> readLines
    |> Seq.map int
    |> Seq.sum

let testsum01 = 
    ["+1";"-2";"+3";"+1"]
    |> Seq.map int
    |> Seq.sum

type Input = int list
type Sums = int list

type FindResult =
| Found of int
| NotFound of Sums

let rec solveForOneList (sums:Sums) (input: Input) =
    match input with
    | [] -> NotFound sums
    | h::t -> 
        let newSum = 
            match sums with
            | [] -> h
            | sum::_ -> sum + h
        if sums |> List.contains newSum then
            Found newSum
        else
            solveForOneList (newSum::sums) t

let rec solveUntilFound (sums:Sums) (input:Input) =
    match solveForOneList sums input with
    | Found i -> i
    | NotFound l -> solveUntilFound l input

let input = 
    ["+1";"-2";"+3";"+1"]
    |> List.map int

let sums : int list = []

input |> solveUntilFound sums

Path.Combine(__SOURCE_DIRECTORY__, "AoC-01-input.txt")
|> File.ReadAllLines
|> Array.toList
|> List.map int
|> solveUntilFound sums
