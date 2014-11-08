﻿
module Parser
open Common
open Grammar
open ParserInternal

open FParsec

let nonnewline (c: char) = not (c = '\r' || c = '\n')
let nonwhite (c: char) = not (int c = 0 || c = ' ' || c = '\r' || c = '\n')
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
let parseArguments =
    let parseNormalArgument = many1Satisfy2 (fun c -> c <> ':' && nonwhite c) nonwhite
    let parseTrailingArgument = pchar ':' >>. many1Satisfy nonnewline
    sepBy (parseTrailingArgument <|> parseNormalArgument) (pchar ' ')
let parseMessage: Grammar.Message parser =
    let first = pchar ':' >>. parsePrefix .>> pchar ' ' |> opt
    let third = pchar ' ' >>. parseArguments
    tuple3 first parseCommand third