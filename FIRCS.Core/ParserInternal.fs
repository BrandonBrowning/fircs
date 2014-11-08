
module ParserInternal
open Common

open FParsec

let parseNick: string parser = regex @"[a-zA-Z][a-zA-Z0-9\-\[\]`^{}\\]*"
let parseHostname: string parser = regex @"[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)+" // irc hostnames only; must be under domain
let parseUser: string parser = regex @"[^ @]+" // rfc says nonspace, but most servers dont seem to allow @
