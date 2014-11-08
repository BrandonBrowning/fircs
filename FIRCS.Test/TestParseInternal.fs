
module TestParseInternal
open CommonTest
open ParserInternal

open FParsec
open NUnit.Framework

[<Test>]
let ``parseHostname example.com``() = assertParses parseHostname "example.com"

[<Test>]
let ``parseHostname site-with-dash.com``() = assertParses parseHostname "site-with-dash.com"

[<Test>]
let ``parseHostname subdomain.example.com``() = assertParses parseHostname "subdomain.example.com"

[<Test>]
let ``parseHostname com``() = assertNotParses parseHostname "com"

[<Test>]
let ``parseNick letters``() = assertParses parseNick "simple"

[<Test>]
let ``parseNick letters then numbers``() = assertParses parseNick "simple50"

[<Test>]
let ``parseNick prefixed number fail``() = assertNotParses parseNick "50simple"

[<Test>]
let ``parseNick dashed``() = assertParses parseNick "foo-bar"

[<Test>]
let ``parseNick every type legal char``() = assertParses parseNick @"everything09-[]\`^{}"

[<Test>]
let ``parseNick fail each simple ascii illegal symbol``() =
    for c in "~!@#$%&*()_=+:;'\",./<>?" do
        sprintf "illegal%c" c |> assertNotParses parseNick