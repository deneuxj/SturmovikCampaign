module Campaign.WatchLogs

open System.IO
open FSharp.Control
open System.Text.RegularExpressions
open System
open System.Collections.Concurrent
open System.Threading
open System.Diagnostics

let private logger = NLog.LogManager.GetCurrentClassLogger()

/// <summary>
/// Indicate if a line from a log comes from an old file, or from a file created since log monitoring started.
/// </summary>
type LogData<'T when 'T :> obj> =
    | Old of 'T
    | Fresh of 'T
with
    override this.ToString() =
        match this with
        | Old x
        | Fresh x -> x :> obj
        |> string

    member this.Data =
        match this with
        | Old x
        | Fresh x -> x

module LogData =
    let map f data =
        match data with
        | Old x -> Old(f x)
        | Fresh x -> Fresh(f x)

type System.IO.Directory with
    static member EnumerateFilesAsync(path, filter) =
        asyncSeq {
            let existingFiles =
                Directory.EnumerateFiles(path, filter)
                |> Seq.sortBy (fun file -> File.GetCreationTimeUtc(Path.Combine(path, file)))
                |> List.ofSeq
            logger.Debug("List of existing files")
            logger.Debug(existingFiles)
            let newFiles = ConcurrentQueue<string>(existingFiles)
            // Race condition: we might miss files created after this point, and before watcher is started
            use watcher = new FileSystemWatcher(path, filter)
            use semaphore = new SemaphoreSlim(newFiles.Count)
            watcher.Created.Add(fun ev ->
                try
                    logger.Debug("New log file created: " + ev.Name)
                    newFiles.Enqueue(IO.Path.Combine(path, ev.Name))
                    semaphore.Release() |> ignore
                with exc ->
                    logger.Error("Exception in watcher.Created handler")
                    logger.Debug(exc))
            watcher.EnableRaisingEvents <- true
            let loop() =
                asyncSeq {
                    let! token = Async.CancellationToken
                    while not(token.IsCancellationRequested) do
                        try
                            do! Async.AwaitTask(semaphore.WaitAsync())
                        with exc ->
                            logger.Debug("Exception while awaiting for new log file semaphore")
                            logger.Debug(exc)
                        match newFiles.TryDequeue() with
                        | true, s ->
                            logger.Debug("Yield file " + s)
                            yield s
                        | false, _ ->
                            logger.Debug("No new files in the queue")
                            do! Async.Sleep(30000)
                }
            yield! loop()
            logger.Debug("Log file watcher shutting down")
        }

let watchLogs path baseName startTime =
    asyncSeq {
        let searchPattern = sprintf "%s[*].txt" baseName
        for file in Directory.EnumerateFilesAsync(path, searchPattern) do
            try
                let fileInfo = FileInfo(Path.Combine(path, file))
                if fileInfo.LastWriteTimeUtc > startTime then
                    yield Fresh file
                else
                    yield Old file
            with
            | exc -> logger.Error(sprintf "Failed to get last write time of %s: '%s'" file exc.Message)
    }