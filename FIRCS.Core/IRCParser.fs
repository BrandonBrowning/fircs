module IRCParser
open FParsec

type 'a parser = Parser<'a, unit>

let parseNick: string parser = regex @"[a-zA-Z][a-zA-Z0-9\-\[\]`^{}\\]*"
let parseHostname: string parser = regex @"[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)+" // irc hostnames only; must be under domain