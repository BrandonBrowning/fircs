
module Grammar

type Message = Prefix option * Command * Parameters

and Prefix =
    | PrefixTarget of string
    | PrefixServer of string
    | PrefixNick of Nick * User option * Host option
and Command = string
and Parameters = string list

and Nick = string
and User = string
and Host = string