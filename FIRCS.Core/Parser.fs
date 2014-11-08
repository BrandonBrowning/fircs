
module Parser
open Common
open Grammar
open ParserInternal

open FParsec

let nonwhite (c: char) = let x = int c in not (x = 0x20 || x = 0x0 || x = 0xD || x = 0xA)
let parseNonwhite: char parser = satisfy nonwhite

let parseServer: Server parser = parseHostname
let parseNick: Nick parser = parseNick
let parsePrefixUser: PrefixUser parser = parseUser
let parsePrefixHost: PrefixHost parser = parseHostname

let parsePrefixServer: PrefixServer parser = parseServer
let parsePrefixNick: PrefixNick parser = 
    let second = pchar '!' >>. parsePrefixUser |> opt
    let third = pchar '@' >>. parsePrefixHost |> opt
    tuple3 parseNick second third

let parsePrefix: Prefix parser = 
    choice [
        parsePrefixServer |>> Prefix.PrefixServer;
        parsePrefixNick |>> Prefix.PrefixNick
    ]
let parseCommand: parser = regex @"[a-zA-Z]+|\d{3}"
let parseParameterlessMessage: Grammar.Message parser =
    let first = pchar ':' >>. parsePrefix .>> pchar ' ' |> opt
    first .>>. parseCommand |>> fun (prefix, command) -> (prefix, command, [])