
module Program
open FParsec
open Parser

open System
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading

let getIrcSocket (host: string): Socket option =
    let rec getIrcSocket' (address: IPAddress) =
        let endpoint = new IPEndPoint(address, 6667)
        let socket = new Socket(endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        socket.Connect(endpoint)
        socket.NoDelay <- true
        if socket.Connected then Some socket else None
        
    let dns = Dns.GetHostEntry(host)
    Seq.tryPick getIrcSocket' dns.AddressList

let like (left: string) (right: string) = left.Equals(right, StringComparison.InvariantCultureIgnoreCase)
    
let buffer = Array.create 4048 0uy

let getSplitArray = Array.create 1 "\r\n"
let get (socket: Socket): string array =
    let length = socket.Receive(buffer)
    let text = Encoding.UTF8.GetString(buffer, 0, length)
    let result = text.Split(getSplitArray, StringSplitOptions.RemoveEmptyEntries)

    for r in result do
        Console.WriteLine("< {0}", r)

    result

let send (socket: Socket) (msg: string) =
    Console.WriteLine("> {0}", msg)
    let payload = msg + "\r\n"
    payload |> Encoding.UTF8.GetBytes |> socket.Send |> ignore

[<EntryPoint>]
let main argv = 
    match getIrcSocket "concrete.slashnet.org" with
        | None -> failwith "Could not get a socket"
        | Some(socket) ->
            send socket "USER sirsalad localhost localhost :chickensalad's bot"
            send socket "NICK sirsalad"

            let rec loop() =
                let commands = get socket
                for command in commands do
                    printfn ""
                    match runParserOnString parseMessage () "test" command with
                        | Failure(text, error, state) ->
                            printfn "! Failed to parse the following..\n%s" text
                        | Success(result, state, position) ->
                            let prefix, command, parameters = result
                            if like command "ping" then
                                sprintf "PONG :%s" parameters.[0] |> send socket
                            else if like command "mode" then
                                send socket "JOIN #uakroncs"
                loop()
            loop()

    Console.ReadLine() |> ignore
    0
