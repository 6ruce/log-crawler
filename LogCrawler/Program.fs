// Learn more about F# at http://fsharp.org

open LogCrawler
open LogCrawler.Selector

open System
open System.IO

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> Console.WriteLine "Please provide log query"
    | _ ->
        match Selector.Parse argv.[0] with
        | Ok selector -> Interpreter.run selector (File.ReadAllLines "LogsExample" |> Seq.toList)
        | Error error -> Console.WriteLine (sprintf "Something wrong with a log query: %s" error)
    0
