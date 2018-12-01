open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let vekkSort (numbers) =
    let folder state elmnt =
        match state with
        | [] -> [elmnt]
        | h::_ -> if elmnt < h then state else elmnt::state
    numbers |> Seq.fold folder []

let sum01 = 
    Path.Combine(__SOURCE_DIRECTORY__, "input-vekksort.txt")
    |> readLines
    |> Seq.map int
    |> vekkSort
    |> List.sum

let testsum01 = 
    [5;4;3;6;7;5;2;7;5;1;1;10]
    |> vekkSort
    |> List.sum
