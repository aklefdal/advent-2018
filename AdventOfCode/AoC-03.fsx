open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Rectangle =
    { ID: string
      Left: int
      Top: int
      Width: int
      Height: int }

let parseLine (s:string) =
    // #18 @ 954,514: 28x10
    let parts = s.Split([|'@';',';':';'x'|]) |> Array.map (fun s-> s.Trim())
    { ID = parts.[0]
      Left = int parts.[1]
      Top = int parts.[2]
      Width = int parts.[3]
      Height = int parts.[4] }

type Square =
    { Left: int
      Top: int }

type RectangleSquares =
    { ID: string
      Squares: Square seq }

let squares (r:Rectangle) = seq {
    for x in r.Left .. (r.Left + r.Width - 1) do
        for y in r.Top .. (r.Top + r.Height - 1) do
            yield { Left = x; Top = y }
}

let toRectangleSquares (r:Rectangle) =
    { ID = r.ID
      Squares = r |> squares }

let allSquares (rs:RectangleSquares seq) =
    rs |> Seq.collect (fun r -> r.Squares)

let testResult = 
    [ "#1 @ 1,3: 4x4"
      "#2 @ 3,1: 4x4"
      "#3 @ 5,5: 2x2" ]
    |> Seq.map parseLine
    |> Seq.map toRectangleSquares
    |> allSquares
    |> Seq.countBy id
    |> Seq.filter (fun (key, count) -> count > 1)
    |> Seq.length

Path.Combine(__SOURCE_DIRECTORY__, "AoC-03-input.txt")
|> readLines
|> Seq.map parseLine
|> Seq.map toRectangleSquares
|> allSquares
|> Seq.countBy id
|> Seq.filter (fun (key, count) -> count > 1)
|> Seq.length

// Part 2
let testrectangles = 
    [ "#1 @ 1,3: 4x4"
      "#2 @ 3,1: 4x4"
      "#3 @ 5,5: 2x2" ]
    |> Seq.map parseLine
    |> Seq.map toRectangleSquares

let rectangles =
    Path.Combine(__SOURCE_DIRECTORY__, "AoC-03-input.txt")
    |> readLines
    |> Seq.map parseLine
    |> Seq.map toRectangleSquares

let overlappingSquares = 
    rectangles
    |> allSquares
    |> Seq.countBy id
    |> Seq.filter (fun (key, count) -> count > 1)
    |> Seq.map fst
    |> Seq.toArray

let hasSquareIn (overlappingSquares:Square []) (rs:RectangleSquares) =
    rs.Squares
    |> Seq.exists (fun s -> overlappingSquares |> Array.contains s)

rectangles
|> Seq.filter (hasSquareIn overlappingSquares >> not)
|> Seq.map (fun rs -> rs.ID)