
module Grammar

type Message = Prefix option * Command * Parameters

and Prefix =
    | PrefixServer of PrefixServer
    | PrefixNick of PrefixNick
and Command = string
and Parameters = Parameter list

and PrefixServer = Server
and PrefixNick = Nick * PrefixUser option * PrefixHost option
and Parameter = string

and Server = string
and Nick = string
and PrefixUser = string
and PrefixHost = string