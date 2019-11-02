#I @"..\bin\Debug"

#r "Campaign.dll"
#r "System.Numerics.Vectors.dll"
#r "DataProvider.dll"
#r "ploggy"
#r "FsPickler"

open System
open System.IO
open Campaign.PlaneModel

PlaneData.DumpAll(Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "planedb.yaml"))
