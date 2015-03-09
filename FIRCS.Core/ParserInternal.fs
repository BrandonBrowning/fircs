
module ParserInternal

open Common

open System
open FParsec

let nickUniqueChars = "[]`^{}".ToCharArray() // symbols unique to nick vs server
let parseNick: string parser = regex @"[a-zA-Z][a-zA-Z0-9\-\[\]`^{}]*"

let hostnameUniqueChars = ".".ToCharArray() // symbols unique to server vs nick
let parseHostname: string parser =
    let legal c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-'
    sepBy1 (many1Satisfy legal) (pchar '.')
        |>> fun strs -> String.Join(".", strs)

let parseUser: string parser = regex @"[^ @]+" // rfc says nonspace, but most servers dont seem to allow @
