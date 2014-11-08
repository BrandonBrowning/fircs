
module Common

open FParsec

type 'a parser = Parser<'a, unit>
type parser = string parser