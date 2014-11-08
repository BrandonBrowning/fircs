module ParseTest

open FParsec
open IRCParser
open NUnit.Framework

let assertParses p str = 
    match runParserOnString p () "test" str with
        | Failure(text, error, state) -> Assert.Fail text
        | Success(result, state, position) -> ()

let assertNotParses p str = 
    match runParserOnString p () "test" str with
        | Failure(text, error, state) -> ()
        | Success(result, state, position) -> Assert.Fail "Expected parser to fail"

[<Test>] let ``parseHostname pass: example.com``() = assertParses parseHostname "example.com"
[<Test>] let ``parseHostname pass: site-with-dash.com``() = assertParses parseHostname "site-with-dash.com"
[<Test>] let ``parseHostname pass: subdomain.example.com``() = assertParses parseHostname "subdomain.example.com"
[<Test>] let ``parseHostname fail: com``() = assertNotParses parseHostname "com"

[<Test>] let ``parseNick pass: simple``() = assertParses parseNick "simple"
[<Test>] let ``parseNick pass: simple50``() = assertParses parseNick "simple50"
[<Test>] let ``parseNick fail: 50simple``() = assertNotParses parseNick "50simple"
[<Test>] let ``parseNick pass: foo-bar``() = assertParses parseNick "foo-bar"
[<Test>] let ``parseNick pass: everything09-[]\`^{}``() = assertParses parseNick @"everything09-[]\`^{}"
[<Test>] let ``parseNick fail: illegal chars: ~!@#$%&*()-_=+:;'",./<>?``() =
    for c in "~!@#$%&*()-_=+:;'\",./<>?" do
        sprintf "illegal%c" c |> assertParses parseNick