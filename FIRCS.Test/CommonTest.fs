
module CommonTest

open FParsec
open NUnit.Framework

let assertEqual<'a> (left: 'a) (right: 'a) = Assert.AreEqual(left, right)
let assertNotEqual<'a> (left: 'a) (right: 'a) = Assert.AreNotEqual(left, right)

let assertParses p str = 
    match runParserOnString (p .>> eof) () "test" str with
        | Failure(text, error, state) -> Assert.Fail text
        | Success(result, state, position) -> printfn "%A" result

let assertNotParses p str = 
    match runParserOnString (p .>> eof) () "test" str with
        | Failure(text, error, state) -> printfn "%A" text
        | Success(result, state, position) -> sprintf "Expected parser to fail; got %A" result |> Assert.Fail

let assertParseMatches p str o = 
    match runParserOnString (p .>> eof) () "test" str with
        | Failure(text, error, state) -> Assert.Fail text
        | Success(result, state, position) -> assertEqual result o

let assertParseNotMatches p str o = 
    match runParserOnString (p .>> eof) () "test" str with
        | Failure(text, error, state) -> Assert.Fail text
        | Success(result, state, position) -> assertNotEqual result o