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
            let! token = Async.CancellationToken
            let existingFiles =
                Directory.EnumerateFiles(path, filter)
                |> Seq.sortBy (fun file -> File.GetCreationTimeUtc(Path.Combine(path, file)))
                |> List.ofSeq
            // Race condition: we might miss files created after this point, and before watcher is started
            logger.Debug("List of existing files")
            logger.Debug(existingFiles)
            let newFiles = ConcurrentQueue<string>(existingFiles)
            // Avoid adding the same file multiple times to the queue
            let seen = System.Collections.Generic.HashSet<string>(existingFiles)
            let seenLock = obj()
            use semaphore = new SemaphoreSlim(newFiles.Count)
            // Repeatedly start new FileSystemWatcher, use them for 5 minutes, then throw away.
            // FileSystemWatchers tend to stop responding after a while, for unknown reasons.
            // Starting a new one every 5 minutes works around that problem.
            let rec producer(stopPrev) =
                async {
                    let watcher = new FileSystemWatcher(path, filter)
                    watcher.Created.Add(fun ev ->
                        if token.IsCancellationRequested then
                            watcher.EnableRaisingEvents <- false
                        else
                        try
                            logger.Debug("Log file written: " + ev.Name)
                            let path = IO.Path.Combine(path, ev.Name)
                            lock seenLock (fun () ->
                                if not (seen.Contains(path)) then
                                    seen.Add(path) |> ignore
                                    newFiles.Enqueue(path)
                                    try
                                        semaphore.Release() |> ignore
                                    with
                                    | :? ObjectDisposedException ->
                                        watcher.EnableRaisingEvents <- false
                                    )
                        with exc ->
                            logger.Error("Exception in watcher.Created handler")
                            logger.Debug(exc))
                    watcher.EnableRaisingEvents <- true
                    logger.Debug("New FileSystemWatcher started")
                    // Stop previous file system watcher after starting the new one, so that no log
                    // file creation may happen while no watcher is active.
                    stopPrev()
                    do! Async.Sleep(300000)
                    watcher.EnableRaisingEvents <- false
                    if not(token.IsCancellationRequested) then
                        return! producer(fun() -> watcher.Dispose(); logger.Debug("Previous FileSystemWatcher stopped"))
                    else
                        watcher.Dispose()
                        logger.Debug("Last FileSystemWatcher stopped")
                }
            let consumer() =
                asyncSeq {
                    while not(token.IsCancellationRequested) do
                        try
                            do! Async.AwaitTask(semaphore.WaitAsync())
                        with exc ->
                            logger.Debug("Exception while awaiting for new log file semaphore")
                            logger.Debug(exc)
                        match newFiles.TryDequeue() with
                        | true, s ->
                            lock seenLock (fun () -> seen.Remove(s) |> ignore)
                            logger.Debug("Yield file " + s)
                            yield s
                        | false, _ ->
                            logger.Debug("No new files in the queue")
                            do! Async.Sleep(30000)
                }
            // Start the producer loop
            Async.Start(producer(ignore), token)
            // Wait for one file to be detected, but do nothing with it. Then start the consumer. We
            // want to yield a file after the next file has been created, to avoid read/write conflicts.
            do! Async.AwaitTask(semaphore.WaitAsync())
            yield! consumer()
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