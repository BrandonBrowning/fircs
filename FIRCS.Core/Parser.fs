
module Parser

open Common
open Grammar
open ParserInternal

open FParsec

let nonnewline (c: char) = not (c = '\r' || c = '\n')
let nonwhite (c: char) = not (int c = 0 || c = ' ' || c = '\r' || c = '\n')
let parseNonwhite: char parser = satisfy nonwhite

let parsePrefix: Prefix parser =
    let parsePrefixNick = tuple3 parseNick (pchar '!' >>. parseUser |> opt) (pchar '@' >>. parseHostname |> opt)
    let followedBySpace = followedByString " " <|> followedBy eof
    choice [
        (parsePrefixNick .>> followedBySpace) |>> PrefixNick |> attempt;
        (parseHostname .>> followedBySpace) |>> PrefixServer
    ] |>> function
        | PrefixNick(target, None, None) ->
            if target.IndexOfAny(hostnameUniqueChars) <> -1 then
                PrefixServer(target)
            else if target.IndexOfAny(nickUniqueChars) <> -1 then
                PrefixNick(target, None, None)
            else
                PrefixTarget(target)
        | other -> other
                
let parseCommand: parser = regex @"[a-zA-Z]+|\d{3}"
let parseArguments =
    let parseNormalArgument = many1Satisfy2 (fun c -> c <> ':' && nonwhite c) nonwhite
    let parseTrailingArgument = pchar ':' >>. many1Satisfy nonnewline
    sepBy (parseTrailingArgument <|> parseNormalArgument) (pchar ' ')
let parseMessage: Grammar.Message parser =
    let first = pchar ':' >>. parsePrefix .>> pchar ' ' |> opt
    let third = pchar ' ' >>. parseArguments |> opt |>> fun x -> defaultArg x []
    tuple3 first parseCommand third