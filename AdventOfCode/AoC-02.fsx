open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseString (s:string) =
    s.ToCharArray()

let contains (i:int) (cs:char array) =
    let lettersWithCorrectLength =
        cs
        |> Array.countBy id
        |> Array.filter (fun (c, count) -> count = i)
    lettersWithCorrectLength.Length > 0

let twos =
    Seq.filter (contains 2)
    >> Seq.length

let threes =
    Seq.filter (contains 3)
    >> Seq.length


let checksum input =
    (input |> twos) * (input |> threes)

let testResult = 
    [ "abcdef"
      "bababc"
      "abbcde"
      "abcccd"
      "aabcdd"
      "abcdee"
      "ababab" ]
    |> Seq.map parseString
    |> checksum

Path.Combine(__SOURCE_DIRECTORY__, "AoC-02-input.txt")
|> readLines
|> Seq.map parseString
|> checksum
