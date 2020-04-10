module LogCrawler.Interpreter

open System
open System.IO
open System.Text.RegularExpressions
open LogCrawler.Selector

let invoke (selector : Selector) (line : string) =
    let source =
        let splitToWords (text : string) = (text.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)) |> Seq.toList
        match selector.What with
        | Line -> [line]
        | Word -> splitToWords line

    let matched (what : string list) =
        match selector.Matcher with
        | Matches regex ->
            let regex = Regex(regex)
            let maybeMatched = List.map (fun src -> (src, regex.Match src)) what |> List.tryFind (fun (_, matched) -> matched.Success)
            match maybeMatched with
            | Some (self, matched) -> (true, self, matched.Groups |> Seq.toArray)
            | None                 -> (false, String.Empty, Array.empty)
        | Contains searchString ->
            let maybeContains = List.map (fun (src : string) -> (src, src.Contains(searchString))) what |> List.tryFind snd
            match maybeContains with
            | Some (self, _) -> (true, self, Array.empty)
            | None           -> (false, String.Empty, Array.empty)

    let act self (groups : Group[]) =
        let text message =
            match message with
            | RegexGroup number when groups.Length > number -> groups.[number].Value
            | String str -> str
            | Self       -> self
            | _          -> String.Empty
        match selector.Writer with
        | Print message -> Console.WriteLine(text message)
        | WriteToFile (message, FilePath filePath) -> File.AppendAllText (filePath, text message)

    match matched source with
    | (true, self, groups) -> act self groups
    | _ -> ()

let run (selector : Selector) (lines : string list) =
    List.iter (invoke selector) lines
