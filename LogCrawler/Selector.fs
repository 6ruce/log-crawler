module LogCrawler.Selector

open FParsec

type SourceString = Line | Word
type FilePath     = FilePath of string
type Matcher
    = Matches  of string
    | Contains of string
type Message
    = RegexGroup of int
    | String     of string
    | Self
type OutputWriter
    = Print       of Message
    | WriteToFile of (Message * FilePath)

type Selector =
    { What : SourceString
      Matcher : Matcher
      Writer : OutputWriter }

    static member Parse (source : string) =
        let str text = pstring text .>> spaces

        let line = str "line" >>% Line
        let word = str "word" >>% Word
        let sourceString = line <|> word

        // Not production ready, deal with \" and \/ screening
        // see https://www.quanttec.com/fparsec/tutorial.html#parsing-json
        // for escaping string examples
        let stringLiteral = (between (str "\"") (str "\"") (manySatisfy ((<>) '"') )) .>> spaces
        let regexLiteral  = (between (str "/")  (str "/")  (manySatisfy ((<>) '/') )) .>> spaces
        let group         = (between (str "(")  (str ")")  (pint32))                  .>> spaces |>> RegexGroup

        let matches  = str "matches"  >>. regexLiteral  |>> Matches
        let contains = str "contains" >>. stringLiteral |>> Contains
        let matcher  = matches <|> contains

        let self    = str "self"    >>% Self
        let strings = stringLiteral |>> String
        let message = group <|> strings <|> self

        let print   = str "print" >>. message |>> Print
        let write   = str "write" >>. message
        let toWhere = str "to"    >>. restOfLine false

        let writeToFile = write .>>. toWhere |>> (fun (msg, path) -> WriteToFile (msg, FilePath path))
        let writer      = print <|> writeToFile

        let parser = sourceString .>>. matcher .>>. writer

        match run parser source with
        | Success (((source, matcher), writer), _, _) ->
            Result.Ok
                { What = source
                  Matcher = matcher
                  Writer = writer }
        | Failure (error, _, _)  -> Result.Error error
