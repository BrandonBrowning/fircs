
module TestParse
open CommonTest
open Grammar
open Parser
open ParserInternal

open FParsec
open NUnit.Framework

[<Test>]
let ``[parsePrefix] simple word is a target``() =
    assertParseMatches parsePrefix
        "nick"
        (PrefixTarget("nick"))

[<Test>]
let ``[parsePrefix] word and user is a nick``() =
    assertParseMatches parsePrefix
        "nick!user"
        (PrefixNick("nick", Some "user", None))

[<Test>]
let ``[parsePrefix] word, user, and host is a nick``() =
    assertParseMatches parsePrefix
        "nick!user@host"
        (PrefixNick("nick", Some "user", Some "host"))

[<Test>]
let ``[parsePrefix] word, user, and domained host is a nick``() =
    assertParseMatches parsePrefix
        "nick!user@host.com"
        (PrefixNick("nick", Some "user", Some "host.com"))

[<Test>]
let ``[parsePrefix] word with domain is a server``() =
    assertParseMatches parsePrefix
        "server.com"
        (PrefixServer("server.com"))

[<Test>]
let ``[parsePrefix] word with subdomain is a server``() =
    assertParseMatches parsePrefix
        "sub.server.com"
        (PrefixServer("sub.server.com"))

[<Test>]
let ``[parsePrefix] word with any unique nick char is a nick``() =
    for c in nickUniqueChars do
        let text = sprintf "nick%c" c
        assertParseMatches parsePrefix
            text
            (PrefixNick(text, None, None))

[<Test>]
let ``[parseArguments] one normal``() =
    assertParseMatches parseArguments
        "foo"
        ["foo"]

[<Test>]
let ``[parseArguments] two normal``() =
    assertParseMatches parseArguments
        "foo bar"
        ["foo"; "bar"]

[<Test>]
let ``[parseArguments] two normal and trailing``() =
    assertParseMatches parseArguments
        "foo bar :baz biz"
        ["foo"; "bar"; "baz biz"]

[<Test>]
let ``[parseArguments] just trailing``() =
    assertParseMatches parseArguments
        ":baz"
        ["baz"]

[<Test>]
let ``[parseMessage] prefix nick host without top level domain``() =
    assertParseMatches parseMessage
        ":nick!~user@host FOO"
        (Some(PrefixNick("nick", Some "~user", Some "host")), "FOO", [])
    
[<Test>]
let ``[parseMessage] ping with data``() =
    assertParseMatches parseMessage
        ":nick PING :abc123"
        (Some(PrefixTarget("nick")), "PING", ["abc123"])
    
[<Test>]
let ``[parseMessage] all fields (prefix nick)``() =
    assertParseMatches parseMessage
        ":NickServ!services@services.slashnet.org NOTICE nick :Your nickname isn't registered."
        (Some(PrefixNick("NickServ", Some "services", Some "services.slashnet.org")), "NOTICE", ["nick"; "Your nickname isn't registered."])
    
[<Test>]
let ``[parseMessage] all fields (prefix server)``() =
    assertParseMatches parseMessage
        ":example.com NOTICE nick :Your nickname isn't registered."
        (Some(PrefixServer("example.com")), "NOTICE", ["nick"; "Your nickname isn't registered."])
    
[<Test>]
let ``[parseMessage] all fields (prefix target)``() =
    assertParseMatches parseMessage
        ":foo NOTICE nick :Your nickname isn't registered."
        (Some(PrefixTarget("foo")), "NOTICE", ["nick"; "Your nickname isn't registered."])