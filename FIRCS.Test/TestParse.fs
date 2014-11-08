
module TestParse
open CommonTest
open Grammar
open Parser

open FParsec
open NUnit.Framework

[<Test>]
let ``parsePrefixNick nick``() =
    assertParseMatches parsePrefixNick "foobar"
        ("foobar", None, None)

[<Test>]
let ``parsePrefixNick nick and user``() =
    assertParseMatches parsePrefixNick "foobar!~foobar"
        ("foobar", Some "~foobar", None)

[<Test>]
let ``parsePrefixNick nick, user, and host``() =
    assertParseMatches parsePrefixNick "foobar!~foobar@example.com"
        ("foobar", Some "~foobar", Some "example.com")

[<Test>]
let ``parsePrefix nick full``() =
    assertParseMatches parsePrefix "foobar!~foobar@example.com"
        (PrefixNick ("foobar", Some "~foobar", Some "example.com"))
 
[<Test>]
let ``parsePrefix domain``() =
    assertParseMatches parsePrefix "example.com" (Prefix.PrefixServer "example.com")

[<Test>]
let ``parsePrefix domain with subdomain``() =
    assertParseMatches parsePrefix "domain.example.com" (Prefix.PrefixServer "domain.example.com")

[<Test>]
let ``parseArguments one normal``() =
    assertParseMatches parseArguments "foo" ["foo"]

[<Test>]
let ``parseArguments two normal``() =
    assertParseMatches parseArguments "foo bar" ["foo"; "bar"]

[<Test>]
let ``parseArguments two normal and trailing``() =
    assertParseMatches parseArguments "foo bar :baz biz" ["foo"; "bar"; "baz biz"]

[<Test>]
let ``parseArguments just trailing``() =
    assertParseMatches parseArguments ":baz" ["baz"]

// ident shouldn't have @, and host can't have an @
[<Test>]
let ``parsePrefixNick @ sign in user fails``() =
    assertNotParses parsePrefixNick "foobar!~foo@bar@cloak-34A93E1F.cl.ri.cox.net"
    
[<Test>]
let ``parseMessage prefix nick ping with data``() =
    assertParseMatches parseMessage ":nick!~user@host.com PING :abc123"
        (("nick", Some "~user", Some "host.com") |> PrefixNick |> Some, "PING", ["abc123"])
    
[<Test>]
let ``parseMessage prefix nick notice nick with message data``() =
    assertParseMatches parseMessage ":NickServ!services@services.slashnet.org NOTICE nick :Your nickname isn't registered."
        (("NickServ", Some "services", Some "services.slashnet.org") |> PrefixNick |> Some, "NOTICE", ["nick"; "Your nickname isn't registered."])
