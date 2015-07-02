module SturmovikMission.DataProvider.FileWithTime

open System

type File = 
    { Name : string
      LastModified : DateTime option }
    static member FromFile(name : string) = 
        try 
            { Name = name
              LastModified = Some(System.IO.File.GetLastWriteTimeUtc(name)) }
        with _ -> 
            { Name = name
              LastModified = None }

