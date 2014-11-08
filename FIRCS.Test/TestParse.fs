
module TestParse
open CommonTest
open Grammar
open Parser

open FParsec
open NUnit.Framework

[<Test>] let ``(parsePrefixNick match) foobar``() =
    assertParseMatches parsePrefixNick "foobar"
        ("foobar", None, None)

[<Test>] let ``(parsePrefixNick match) foobar!~foobar``() =
    assertParseMatches parsePrefixNick "foobar!~foobar"
        ("foobar", Some "~foobar", None)

[<Test>] let ``(parsePrefixNick match) foobar!~foobar@example.com``() =
    assertParseMatches parsePrefixNick "foobar!~foobar@example.com"
        ("foobar", Some "~foobar", Some "example.com")

[<Test>] let ``(parsePrefix match) foobar!~foobar@example.com``() =
    assertParseMatches parsePrefix "foobar!~foobar@example.com"
        (PrefixNick ("foobar", Some "~foobar", Some "example.com"))

[<Test>] let ``(parsePrefix match) domain.example.com``() =
    assertParseMatches parsePrefix "domain.example.com" (Prefix.PrefixServer "domain.example.com")

[<Test>] let ``(parseArguments match) foo bar :baz biz``() =
    assertParseMatches parseArguments "foo bar :baz biz" ["foo"; "bar"; "baz biz"]

// ident shouldn't have @, and host can't have an @
[<Test>] let ``(parsePrefixNick fail) foobar!foo@bar@cloak-34A93E1F.cl.ri.cox.net``() =
    assertNotParses parsePrefixNick "foobar!~foo@bar@cloak-34A93E1F.cl.ri.cox.net"
    
[<Test>] let ``(parseMessage match) :nick!user@host.com PING :abc123``() =
    assertParseMatches parseMessage ":nick!~user@host.com PING :abc123"
        (("nick", Some "~user", Some "host.com") |> PrefixNick |> Some, "PING", ["abc123"])
    
[<Test>] let ``(parseMessage match) :NickServ!services@services.slashnet.org NOTICE nick :Your nickname isn't registered.``() =
    assertParseMatches parseMessage ":NickServ!services@services.slashnet.org NOTICE nick :Your nickname isn't registered."
        (("NickServ", Some "services", Some "services.slashnet.org") |> PrefixNick |> Some, "NOTICE", ["nick"; "Your nickname isn't registered."])
