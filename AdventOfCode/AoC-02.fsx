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

// Part 2

let compareCharArrays (cs1:char []) (cs2:char []) =
    let similarChars =
        cs1
        |> Array.zip cs2
        |> Array.filter (fun (c1,c2) -> c1 = c2)
        
    if similarChars.Length = cs1.Length - 1 then
        similarChars
        |> Array.map fst
        |> (fun cs -> System.String.Concat(cs))
        |> Some
    else
        None    

let rec findMatch (e:char []) (s: char [] list) =
    match s with
    | [] -> None
    | h::t ->
        match compareCharArrays e h with
        | Some solution -> Some solution
        | None -> findMatch e t 

let rec solve (s: char [] list) =
    match s with
    | [] -> None
    | h::t ->
        match findMatch h t with
        | Some solution -> Some solution
        | None -> solve t    

let testResult2 =
    [ "abcde"
      "fghij"
      "klmno"
      "pqrst"
      "fguij"
      "axcye"
      "wvxyz" ]
    |> List.map parseString
    |> solve

Path.Combine(__SOURCE_DIRECTORY__, "AoC-02-input.txt")
|> File.ReadAllLines
|> Array.map parseString
|> Array.toList
|> solve
