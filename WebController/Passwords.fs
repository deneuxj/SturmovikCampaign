module Campaign.Passwords

open FSharp.Json
open System.Security.Cryptography
open System

/// Mapping from user names to password hashes
type Passwords =
    {
        UserPasswords : Map<string, byte[]>
    }
with
    static member LoadFromFile(path : string) =
        let json = IO.File.ReadAllText(path)
        Json.deserialize json

    member this.Save(path : string) =
        use file = IO.File.CreateText(path)
        let json = Json.serialize this
        file.Write(json)

    member this.Validate(user : string, password : string) =
        match this.UserPasswords.TryFind user with
        | Some hash ->
            use algo = HashAlgorithm.Create("SHA256")
            let hashed = algo.ComputeHash(Text.Encoding.UTF8.GetBytes(password))
            hash = hashed
        | None ->
            false

    member this.SetPassword(user : string, password : string) =
        use algo = HashAlgorithm.Create("SHA256")
        let hashed = algo.ComputeHash(Text.Encoding.UTF8.GetBytes(password))
        { this with
            UserPasswords = this.UserPasswords.Add(user, hashed)
        }

/// Synchronize passwords with file on disc and provides thread-safe access
type PasswordsManager() =
    let theLock = obj()
    let path = IO.Path.GetFullPath("passwords.json")
    let mutable passwords =
        if IO.File.Exists(path) then
            Passwords.LoadFromFile path
        else
            { UserPasswords = Map.empty }
    let mutable lastCheck = DateTime.UtcNow

    let update() =
        let needsUpdate =
            try
                let lastModif = IO.File.GetLastWriteTimeUtc(path)
                lastModif > lastCheck
            with _ -> true
        if needsUpdate && IO.File.Exists(path) then
            try
                passwords <- Passwords.LoadFromFile(path)
            with _ ->
                failwith "Updated needed but failed"

    member this.SetPassword(user : string, password : string) =
        lock theLock (fun () ->
            try
                update()
                passwords <- passwords.SetPassword(user, password)
                passwords.Save(path)
                Ok()
            with _ ->
                Error "Failed to set password"
        )

    member this.Validate(user : string, password : string) =
        lock theLock (fun () ->
            try
                update()
            with _ -> ()
            passwords.Validate(user, password)
        )


