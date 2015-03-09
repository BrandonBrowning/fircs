
module Common

open System
open FParsec

type 'a parser = Parser<'a, unit>
type parser = string parser

let like (left: string) (right: string) = left.Equals(right, StringComparison.InvariantCultureIgnoreCase)