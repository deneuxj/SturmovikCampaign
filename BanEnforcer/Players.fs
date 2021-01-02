module Coconutside.BanEnforcer.Players

open System
open FSharp.Json
open System.Security.Cryptography
open System.Text

type HashedGuid = HashedGuid of string
with
    member this.String =
        let (HashedGuid s) = this
        s

/// A banned player
type Player =
    {
        // Game GUID. Don't expose
        Guid : string
        // Hashed game GUID (SHA256, Base64-encoded)
        HashedGuid : HashedGuid
        BannedUntil : DateTime
        DisplayNames : string list
    }

let hashGuid (guid : string) =
    use hasher = HashAlgorithm.Create("SHA256")
    guid
    |> Encoding.ASCII.GetBytes
    |> hasher.ComputeHash
    |> System.Convert.ToBase64String
    |> HashedGuid

/// A database of banned players
type PlayerDb =
    {
        Players : Player list
    }
with
    /// Add or update a ban
    member this.Add(guid : string, duration : TimeSpan, now : DateTime) =
        let existing =
            this.Players
            |> List.tryFind (fun player -> player.Guid = guid)
        let player =
            existing
            |> Option.defaultValue {
                Guid = guid
                HashedGuid = hashGuid guid
                BannedUntil = now + duration
                DisplayNames = []
            }
        { this with
            Players =
                player :: (
                    this.Players
                    |> List.filter (fun player -> player.Guid <> guid))
        }.Refresh(now)

    /// Clear a ban
    member this.Unban(guid : HashedGuid) =
        { this with
            Players =
                this.Players
                |> List.filter (fun player -> player.HashedGuid <> guid)
        }

    /// Remove players whose ban has expired
    member this.Refresh(now : DateTime) =
        let players =
            this.Players
            |> List.filter (fun player -> player.BannedUntil >= now)
            |> List.sortBy (fun player -> player.Guid)
        { this with
            Players = players
        }

    /// Find players by their display name
    member this.FindPlayers(displayName : string) =
        let exact =
            this.Players
            |> List.filter (fun player -> player.DisplayNames |> List.exists ((=) displayName))
        let approximate() =
            let normalize (s : string) =
                s.ToLowerInvariant().Trim()
                |> String.filter (Char.IsLetterOrDigit)
            let displayName = normalize displayName
            this.Players
            |> List.filter (fun player -> player.DisplayNames |> List.exists (fun s -> normalize s = displayName))
        match exact with
        | [] -> approximate()
        | _ -> exact

    /// Match a list of GUIDs with entries in the ban database
    member this.MatchBans(guids : string list) =
        let guids = List.sort guids
        let rec work (players : Player list, guids : string list) =
            seq {
                match players, guids with
                | [], _ | _, [] -> ()
                | player :: players, guid :: guids ->
                    if player.Guid < guid then
                        yield! work(players, guid :: guids)
                    elif player.Guid > guid then
                        yield! work(player :: players, guids)
                    else
                        assert(player.Guid = guid)
                        yield guid
                        yield! work(players, guids)
            }
        work(this.Players, guids)

    static member Default = { Players = [] }

    /// Try to load the player db from a file. If the file does not exist, return the empty db.
    static member Load(path : string) =
        async {
            if IO.File.Exists(path) then
                let! json = IO.File.ReadAllTextAsync(path)
                return Json.deserialize<PlayerDb> json
            else
                return PlayerDb.Default
        }

    /// Save the db to a file
    member this.Save(path : string) =
        async {
            let json = Json.serialize this
            do! IO.File.WriteAllTextAsync(path, json)
        }