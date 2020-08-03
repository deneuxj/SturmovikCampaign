// RConClient Copyright (C) 2016 Johann Deneux <johann.deneux@gmail.com>
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace RConClient

open System.Net
open System.Text
open System.Web
open System
open System.Diagnostics

type StepPerSecData =
    { Current : float32
      Minimum : float32
      Maximum : float32
      Average : float32
    }

type MyStatus =
    { Status : int
      Authed : bool
    }

type PlayerData =
    { ClientId : int
      Status : int
      Ping : int
      Name : string
      PlayerId : string
      ProfileId : string
    }

exception ConnectionException of unit

/// <summary>
/// Interaction between the web server and DServer.
/// Provides authentication, triggering server inputs...
/// </summary>
type Client(hostname : string, port, login, password) as this =
    inherit Sockets.TcpClient()

    let logger = NLog.LogManager.GetCurrentClassLogger()

    do
        try
            base.Connect(hostname, port)
        with
        | exc ->
            logger.Error(sprintf "Failed to connect to game server: %s" exc.Message)
            raise(ConnectionException())
    let stream = this.GetStream()

    let send(buff, idx, len) = Async.FromBeginEnd((fun (cb, par) -> stream.BeginWrite(buff, idx, len, cb, par)), stream.EndWrite)
    let receive(buff, idx, len) =
        let f(buff, idx, len) = Async.FromBeginEnd((fun (cb, par) -> stream.BeginRead(buff, idx, len, cb, par)), stream.EndRead)
        // Repeatedly read until we have received all requested bytes.
        // Note: Reading from a socket returns a number of bytes up to the requested number, as opposed to waiting until the requested number is available and then returning.
        let rec work readSoFar leftToRead =
            async {
                let! readThisTime = f(buff, readSoFar, leftToRead)
                //Debug.Write(sprintf "receive: %d (%d, %d)" readThisTime readSoFar leftToRead)
                if readThisTime < leftToRead then
                    return! work (readSoFar + readThisTime) (leftToRead - readThisTime)
                else
                    ()
            }
        work idx len

    let encode (cmd : string) =
        let asAscii = Encoding.ASCII.GetBytes(cmd)
        let length = [| byte((asAscii.Length + 1) % 256) ; byte((asAscii.Length + 1) / 256) |]
        let buff = Array.concat [length ; asAscii ; [|0uy|] ]
        buff

    let decodeResponse(response : string) =
        [
            for pair in response.Split('&') do
                match pair.Split('=') with
                | [| key; value |] -> yield (key, value)
                | _ -> failwithf "Could not split key-value pair %s" pair
        ]

    let getResponse(stream : Sockets.NetworkStream) =
        async {
            let buffer : byte[] = Array.zeroCreate 0xffff
            let response = stream.Read(buffer, 0, 2)
            let responseLength = (256 * int buffer.[1]) + int buffer.[0]
            let! response = receive(buffer, 2, responseLength)
            let data =
                if responseLength > 0 then
                    Encoding.ASCII.GetString(buffer, 2, responseLength - 1)
                else
                    ""
            return data
        }

    let parseFloat s =
        System.Single.Parse(s, System.Globalization.CultureInfo.InvariantCulture)

    let parseInt s =
        System.Int32.Parse(s, System.Globalization.CultureInfo.InvariantCulture)

    member this.Auth() =
        async {
            let buff = encode <| sprintf "auth %s %s" login password
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.ServerStatus() =
        async {
            let buff = encode "serverstatus"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            let decoded = decodeResponse response |> dict
            return parseInt decoded.["STATUS"]
        }

    member this.MyStatus() =
        async {
            let buff = encode "mystatus"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            let decoded = decodeResponse response |> dict
            return {
                Status = parseInt decoded.["STATUS"]
                Authed = decoded.["authed"] = "1"
            }
        }

    member this.OpenSds(filename) =
        async {
            let buff =
                sprintf "opensds %s" filename
                |> encode
            do! send(buff, 0, buff.Length)
            return! getResponse stream
        }

    member this.CloseSds() =
        async {
            let buff = encode "closesds"
            do! send(buff, 0, buff.Length)
            return! getResponse stream
        }

    member this.ServerInput(name) =
        async {
            let buff =
                sprintf "serverinput %s" name
                |> encode
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.MessagePlayer(playerId, msg) =
        async {
            let buff =
                sprintf "chatmsg 3 %d %s" playerId msg
                |> encode
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.MessageTeam(teamId, msg) =
        async {
            let buff =
                sprintf "chatmsg 1 %d %s" teamId msg
                |> encode
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.MessageAll(msg) =
        async {
            let buff =
                sprintf "chatmsg 0 0 %s" msg
                |> encode
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.GetSPS() =
        async {
            let buff = encode "spsget"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            let decoded =
                decodeResponse response
                |> dict
            return
                { Current = decoded.["curSps"] |> parseFloat
                  Minimum = decoded.["minSps"] |> parseFloat
                  Maximum = decoded.["maxSps"] |> parseFloat
                  Average = decoded.["avgSps"] |> parseFloat
                }
        }

    member this.ResetSPS() =
        async {
            let buff = encode "spsreset"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.GetPlayerList() =
        async {
            let buff = encode "getplayerlist"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            let pairs = response.Split('&')
            let result =
                pairs
                |> Array.map (fun kvp ->
                    match kvp.Split('=') with
                    | [| key; value |] -> (key, HttpUtility.UrlDecode(value))
                    | _ -> failwithf "Ill-formatted key-value pair %s" kvp)
                |> Array.tryPick (function
                    | "playerList", values ->
                        HttpUtility.UrlDecode(values).Split('|')
                        |> Array.map (fun player -> player.Split(','))
                        |> Some
                    | _ ->
                        None)
                |> Option.map (fun rows ->
                    rows.[1..]
                    |> Array.map(fun row ->
                        { ClientId = Int32.Parse(row.[0])
                          Status = Int32.Parse(row.[1])
                          Ping = Int32.Parse(row.[2])
                          Name = row.[3]
                          PlayerId = row.[4]
                          ProfileId = row.[5]
                        })
                    |> Array.filter(fun data -> data.ClientId <> 0)
                )
            return result
        }

    member this.KickPlayer(playerId) =
        async {
            let buff = encode (sprintf "kick cid %d" playerId)
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.BanPlayer(playerId) =
        async {
            let buff = encode (sprintf "ban cid %d" playerId)
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.Shutdown() =
        async {
            let buff = encode "shutdown"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.CutChatLog() =
        async {
            let buff = encode "cutchatlog"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

    member this.SendStatNow() =
        async {
            let buff = encode "sendstatnow"
            do! send(buff, 0, buff.Length)
            let! response = getResponse stream
            return response
        }

/// <summary>
/// Provides asynchronous sequential access to a client.
/// Allows to use the same client from multiple threads.
/// </summary>
type ClientMessageQueue(hostname, port, login, password) =
    let logger = NLog.LogManager.GetCurrentClassLogger()
    let mutable client = None
    let clientLock = obj()

    let resetClient() =
        lock clientLock (fun () -> client <- None)

    // Mailbox that receives untyped asyncs, executes them and return the result (if any) as an untyped object in a reply channel.
    let rec handleMessage (mb : MailboxProcessor<Async<obj> * AsyncReplyChannel<obj option> option>) =
        async {
            let! msg = mb.Receive()
            match msg with
            | f, None ->
                try
                    let! dummy = f
                    ignore dummy
                with
                | e ->
                    resetClient()
                    logger.Debug(sprintf "Exception thrown by action in ClientMessageQueue.handleMessage: %s" e.Message)
            | f, Some r ->
                try
                    let! untyped = f
                    r.Reply(Some untyped)
                with
                | e ->
                    resetClient()
                    logger.Debug(sprintf "Exception thrown by function in ClientMessageQueue.handleMessage: %s" e.Message)
                    r.Reply(None)
            return! handleMessage mb
        }

    let mb =
        MailboxProcessor.Start(handleMessage)

    /// <summary>
    /// Get the RConClient, or create one if it hasn't been created yet.
    /// </summary>
    member this.Client =
        lock clientLock (fun () ->
            match client with
            | Some client -> client
            | None ->
                let x = new Client(hostname, port, login, password)
                async {
                    let! response = x.Auth()
                    logger.Debug(sprintf "Auth response %s" response)
                } |> Async.RunSynchronously
                client <- Some x
                x)

    /// <summary>
    /// Build an async that simply posts a return-less action and continues.
    /// </summary>
    member this.Start(f: Lazy<Async<unit>>) =
        let untypedF =
            async {
                do! f.Value
                return box()
            }
        async {
            mb.Post(untypedF, None)
        }

    /// <summary>
    /// Build an async that posts a function and waits for the result.
    /// </summary>
    member this.Run(f: Lazy<Async<'T>>) =
        let untypedF =
            async {
                let! x = f.Value
                return box x
            }
        async {
            let! untyped = mb.PostAndAsyncReply(fun reply -> untypedF, Some reply)
            match untyped with
            | Some untyped ->
                return unbox<'T>(untyped) |> Some
            | None ->
                return None
        }

    member this.Dispose() =
        let client = client
        match client with
        | Some client -> client.Close()
        | None -> ()

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()