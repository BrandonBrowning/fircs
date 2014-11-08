
module TestParse
open CommonTest
open Parser

open FParsec
open NUnit.Framework

[<Test>] let ``parsePrefixNick pass> foobar!~foobar@cloak-34A93E1F.cl.ri.cox.net``() =
    assertParses parsePrefixNick "foobar!~foobar@cloak-34A93E1F.cl.ri.cox.net"