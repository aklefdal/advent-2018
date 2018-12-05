open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type ObservedEvent =
| WakeUp
| FallsAsleep
| BeginsDuty of int

type Observation =
    { CompleteDateTime: string
      Minute: int
      Event: ObservedEvent}

type DateStatus = 
    {  }

let parseLine (s:string) =
    // [1518-04-21 00:57] wakes up
    // [1518-09-03 00:12] falls asleep
    // [1518-04-21 00:04] Guard #3331 begins shift
    // [1518-10-29 00:51] falls asleep
    // [1518-06-12 00:02] Guard #3109 begins shift
    let parts = s.Split([|'[';']'|]) |> Array.map (fun s-> s.Trim())
    let dateTime = parts.[1] 
    let minutes = dateTime.Substring(14, 2) |> int
    let text = parts.[2].Split([|' ';'#'|]) |> Array.map (fun s-> s.Trim())
    let event = 
        match text.[0] with
        | "wakes" -> WakeUp
        | "falls" -> FallsAsleep
        | "Guard" -> text.[2] |> int |> BeginsDuty
        | _ -> failwith "Should not happen"

    { CompleteDateTime = dateTime
      Minute = minutes
      Event = event }


let testResult = 
    [ "[1518-11-01 00:00] Guard #10 begins shift"
      "[1518-11-01 00:05] falls asleep"
      "[1518-11-01 00:25] wakes up"
      "[1518-11-01 00:30] falls asleep"
      "[1518-11-01 00:55] wakes up"
      "[1518-11-01 23:58] Guard #99 begins shift"
      "[1518-11-02 00:40] falls asleep"
      "[1518-11-02 00:50] wakes up"
      "[1518-11-03 00:05] Guard #10 begins shift"
      "[1518-11-03 00:24] falls asleep"
      "[1518-11-03 00:29] wakes up"
      "[1518-11-04 00:02] Guard #99 begins shift"
      "[1518-11-04 00:36] falls asleep"
      "[1518-11-04 00:46] wakes up"
      "[1518-11-05 00:03] Guard #99 begins shift"
      "[1518-11-05 00:45] falls asleep"
      "[1518-11-05 00:55] wakes up" ]
    |> List.map parseLine
    |> List.sortBy (fun o -> o.CompleteDateTime)

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