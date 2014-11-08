
module TestParse
open CommonTest
open Parser

open FParsec
open NUnit.Framework

[<Test>] let ``(parsePrefixNick match) foobar!~foobar@example.com``() =
    assertParseMatches parsePrefixNick "foobar!~foobar@example.com"
        ("foobar", Some "~foobar", Some "example.com")

// ident shouldn't have @, and host can't have an @
[<Test>] let ``(parsePrefixNick fail) foobar!foo@bar@cloak-34A93E1F.cl.ri.cox.net``() =
    assertNotParses parsePrefixNick "foobar!~foo@bar@cloak-34A93E1F.cl.ri.cox.net"