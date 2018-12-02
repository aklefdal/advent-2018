open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Point =
    { X: int
      Y: int }

type Vector =
    { P1: Point
      P2: Point }

let inclination (v:Vector) =
    let { P1 = p1; P2 = p2 } = v
    ((p2.Y - p1.Y) |> float) / ((p2.X - p1.X) |> float)

let absInclination (v:Vector) =
    let inc = v |> inclination
    System.Math.Abs(inc)

let parsePoint (s:string) =
    // (446,767)
    let numbers = s.Substring(1, s.Length - 2).Split([|','|]) |> Array.map int
    { X = numbers.[0]
      Y = numbers.[1] }

let parseLine (s:string) =
    // (446,767);(933,757)
    let points = s.Split([|';'|]) |> Array.map parsePoint
    { P1 = points.[0]
      P2 = points.[1]}

let max = 
    Path.Combine(__SOURCE_DIRECTORY__, "input-rain.txt")
    |> readLines
    |> Seq.map parseLine
    |> Seq.map absInclination
    |> Seq.countBy id
    |> Seq.maxBy snd
