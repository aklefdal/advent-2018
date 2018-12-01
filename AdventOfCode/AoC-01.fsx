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

