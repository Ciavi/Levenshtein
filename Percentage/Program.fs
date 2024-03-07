open FSharp.Core
open System

type Levenshtein(s0, s1) = 
    let mutable first : string = s0
    let mutable second : string = s1
    let mutable longest : string = if s0.Length > s1.Length then s0 elif s0.Length < s1.Length then s1 else s1

    member this.First
        with get() = first
        and set(value) = first <- value

    member this.Second
        with get() = second
        and set(value) = second <- value

    member this.Longest
        with get() = longest
        
    member this.Run() = 
        let firstLength : int = first.Length
        let secondLength : int = second.Length
        let mutable distance : int[,] = Array2D.zeroCreate (firstLength + 1) (secondLength + 1)

        if firstLength = 0 then secondLength
        elif secondLength = 0 then firstLength
        else
            for i = 0 to firstLength do
                distance.[i, 0] <- i

            for j = 0 to secondLength do
                distance.[0, j] <- j

            for i = 1 to firstLength do
                for j = 1 to secondLength do
                    let cost : int = if (second.[j - 1] = first.[i - 1]) then 0 else 1
                    distance.[i, j] <- Math.Min(Math.Min((distance.[i - 1, j] + 1), (distance.[i, j - 1] + 1)), (distance.[i - 1, j - 1] + cost))

            let result = distance.[firstLength, secondLength]

            result


Console.WriteLine("---------------------")
Console.WriteLine("Welcome to LevPct 1.0")
Console.WriteLine("---------------------")

Console.WriteLine()

Console.WriteLine(">>>>>>>>>>>>>>>>>>>>>")
Console.Write("firstString ('end' to terminate): ")
let mutable firstString : string = Console.ReadLine()

Console.Write("secondString ('end' to terminate): ")
let mutable secondString : string = Console.ReadLine()
Console.WriteLine("<<<<<<<<<<<<<<<<<<<<<")

Console.WriteLine()

while (firstString <> "end" && secondString <> "end") do
    Console.WriteLine("Computing...")

    Console.WriteLine()

    let levenshtein : Levenshtein = new Levenshtein(firstString, secondString)
    let distance : int = levenshtein.Run()
    let similarity : double = 100.00 - ((double)distance / (double)levenshtein.Longest.Length * 100.00)

    Console.WriteLine("=====================")

    Console.WriteLine($"firstLength: {firstString.Length}")
    Console.WriteLine($"secondLength: {secondString.Length}")
    Console.WriteLine($"longest: {levenshtein.Longest}")
    
    Console.WriteLine("---------------------")

    Console.WriteLine($"distance: {distance}")
    Console.WriteLine($"similarity: {similarity}pct")

    Console.WriteLine("=====================")

    Console.WriteLine()

    Console.WriteLine(">>>>>>>>>>>>>>>>>>>>>")
    Console.Write("firstString ('end' to terminate): ")
    firstString <- Console.ReadLine()

    Console.Write("secondString ('end' to terminate): ")
    secondString <- Console.ReadLine()
    Console.WriteLine("<<<<<<<<<<<<<<<<<<<<<")

    Console.WriteLine()

exit 0