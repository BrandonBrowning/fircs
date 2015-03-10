
module Bot

open Core
open FParsec
open Grammar
open Magic
open Parser

open System
open System.Net
open System.Net.Sockets
open System.Text
open System.Text.RegularExpressions
open System.Threading

let messageToString (message: Message): string =
    let prefix, command, args = message

    let prefixStr = match prefix with
        | None -> ""
        | Some(prefix) ->
            let body = match prefix with
                | PrefixTarget(str) -> str
                | PrefixServer(str) -> str
                | PrefixNick(nick, user, host) -> 
                    let user_str = match user with None -> "" | Some(x) -> sprintf "!%A" x
                    let host_str = match host with None -> "" | Some(x) -> sprintf "@%A" x
                    nick + user_str + host_str
            sprintf ":%s " body

    let argsStr =
        let rec argsStr' args (acc: string list) = 
            match args with
                | [] -> if acc.Length = 0 then "" else " " + String.Join(" ", acc |> List.rev)
                | h::[] -> argsStr' [] ((":" + h)::acc)
                | h::t -> argsStr' t (h::acc)
        argsStr' args []

    sprintf "%s%s%s" prefixStr command argsStr

let makeIRCSocket (host: string): Socket option =
    let rec getIrcSocket' (address: IPAddress) =
        let endpoint = new IPEndPoint(address, 6667)
        let socket = new Socket(endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        socket.Connect(endpoint)
        socket.NoDelay <- true
        if socket.Connected then Some socket else None

    let dns = Dns.GetHostEntry(host)
    Seq.tryPick getIrcSocket' dns.AddressList
    
let buffer = Array.create 4048 0uy

let getSplitArray = Array.create 1 "\r\n"
let getSocketLines (socket: Socket): string array =
    let length = socket.Receive(buffer)
    let text = Encoding.UTF8.GetString(buffer, 0, length)
    text.Split(getSplitArray, StringSplitOptions.RemoveEmptyEntries)

let sendSocketLine (socket: Socket) (str: string) =
    let payload = str + "\r\n"
    payload |> Encoding.UTF8.GetBytes |> socket.Send |> ignore

let getSocketMessages (socket: Socket): Message seq =
    seq {
        for line in getSocketLines socket do
            match runParserOnString parseMessage () "irc input" line with
                | Failure(text, error, state) ->
                    printfn "! Failed to parse the following..\n%s" text
                | Success(result, state, position) ->
                    printfn "< %s" line
                    yield result
    }

let sendSocketMessage (socket: Socket) (message: Message) =
    let line = messageToString message
    printfn "> %s" line
    line  + "\r\n" |> sendSocketLine socket

let outMessage (command: string) (args: string list): Message = (None, command.ToUpper(), args)
let userMessage user = outMessage "user" [user; "localhost"; "localhost"; "chickensalad's bot"]
let nickMessage nick = outMessage "nick" [nick]
let pongMessage arg = outMessage "pong" [arg]
let joinMessage channel = outMessage "join" [channel]
let privMessage target message = outMessage "privmsg" [target; message]
let channelMessage channel message = outMessage "privmsg" [channel; message]

let nick = "sirsalad"
let user = nick
let channel = "#uakroncs"

let keepAliveModule ((prefixOpt, command, args): Message): Message seq =
    seq {
        if like command "ping" then
            yield pongMessage args.[0]
        else if like command "mode" then
            yield joinMessage channel
    }

let echoModule ((prefixOpt, command, args): Message): Message seq =
    seq {
        if like command "privmsg" then
            match prefixOpt with
                | Some(PrefixNick(from_nick, _, _)) ->
                    let target = args.[0]
                    let message = args.[1]

                    if target = nick then
                        yield privMessage from_nick message
                    else if target = channel then
                        let nick_prefix = sprintf "%s:" nick
                        if message.StartsWith(nick_prefix) then
                            let response = message.Substring(nick_prefix.Length).Trim()
                            yield sprintf "%s: %s" from_nick response |> channelMessage channel
                | _ -> ()
    }

let shittyMagicModule ((prefixOpt, command, args): Message): Message seq =
    let extractKeywords (text: string): string seq =
        seq {
            for m in Regex.Matches(text, "\|([\w_,\-' ]{3,})\|") do
                if m.Groups.Count = 2 then
                    yield m.Groups.[1].Value
        }

    seq {
        if like command "privmsg" then
            match prefixOpt with
                | Some(PrefixNick(from_nick, _, _)) ->
                    let target = args.[0]
                    let message = args.[1]

                    if target = nick then
                        yield privMessage from_nick message
                    else if target = channel then
                        let keywords = extractKeywords message |> List.ofSeq
                        if keywords.Length > 0 then
                            for keyword in keywords do
                                match getRulesTextLine keyword with
                                    | Ok(response) -> yield response |> channelMessage channel
                                    | failure -> yield sprintf "Failed to show %s due to %A" keyword failure |> channelMessage channel
                | _ -> ()
    }

let execBot() =
    match makeIRCSocket "concrete.slashnet.org" with
        | None -> failwith "Could not get a socket"
        | Some(socket) ->
            let send = sendSocketMessage socket
            let gets() = getSocketMessages socket

            userMessage user |> send
            nickMessage nick |> send

            let modules = [keepAliveModule; echoModule; shittyMagicModule]

            let rec loop() =
                for inMessage in gets() do
                    for ircModule in modules do
                        for outMessage in ircModule inMessage do
                            send outMessage
                loop()
            loop()