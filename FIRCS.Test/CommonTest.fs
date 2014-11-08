
module CommonTest

open FParsec
open NUnit.Framework

let assertParses p str = 
    match runParserOnString (p .>> eof) () "test" str with
        | Failure(text, error, state) -> Assert.Fail text
        | Success(result, state, position) -> printfn "%A" result

let assertNotParses p str = 
    match runParserOnString (p .>> eof) () "test" str with
        | Failure(text, error, state) -> printfn "%A" text
        | Success(result, state, position) -> sprintf "Expected parser to fail; got %A" result |> Assert.Fail
