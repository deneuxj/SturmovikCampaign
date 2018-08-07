module Campaign.WatchLogs

open System.IO
open FSharp.Control
open System.Text.RegularExpressions
open System
open System.Collections.Concurrent
open System.Threading
open System.Diagnostics

let private farInThePast = DateTime(0L)

/// <summary>
/// Monitor a directory containing logs, and return every new line in the most recent log file.
/// Also returns an action that stops monitoring.
/// </summary>
/// <param name="cleanLogs">If true, delete old log files whenever a new log file is created.</param>
/// <param name="path">Path to the directory containing the logs</param>
/// <param name="filter">Filter matching log files.</param>
/// <param name="firstFile">Optional name of a file whose lines to return first.</param>
let watchLogs(cleanLogs : bool, path, filter, firstFile, cancelToken : CancellationToken) =
    let currentFile =
        firstFile
        |> Option.map (fun f -> (f, 0, farInThePast))
    // let stackTrace = StackTrace(true)
    asyncSeq {
        // Watch for newly created files, add them in a queue
        let newFiles = ConcurrentQueue<string>()
        let w = new FileSystemWatcher(path, filter)
        w.Created.Add(fun ev -> newFiles.Enqueue(ev.FullPath))
        w.EnableRaisingEvents <- true
        // Process lines from the current log, then move on to the next log file, if any
        let rec do1(currentFile : (string * int * DateTime) option) =
            asyncSeq {
                // Check for new lines from the current log
                let lines, currentFile =
                    match currentFile with
                    | Some(file, skip, lastRead) ->
                        let lastModified = File.GetLastWriteTime(file)
                        if lastModified > lastRead then
                            try
                                let lines =
                                    File.ReadAllLines(file)
                                lines.[skip..], Some(file, lines.Length, lastModified)
                            with
                            | _ -> [||], currentFile
                        else
                            [||], currentFile
                    | None ->
                        [||], currentFile
                // Report these lines
                for line in lines do
                    yield line
                // Check for new log file
                match newFiles.TryDequeue() with
                | true, newFile ->
                    // Make sure we don't try to read a newly created log file while it's being created
                    if newFiles.IsEmpty then
                        do! Async.Sleep(60000)
                    if cleanLogs then
                        match currentFile with
                        | Some(file, _, _) ->
                            try
                                File.Delete(file)
                            with
                            | _ -> ()
                        | None ->
                            ()
                    yield! do1(Some(newFile, 0, farInThePast))
                | false, _ ->
                    // No new file, sleep for 1s and try again
                    if not cancelToken.IsCancellationRequested then
                        do! Async.Sleep(1000)
                        yield! do1(currentFile)
                    else
                        w.Dispose()
            }
        yield! do1 currentFile
    }

/// <summary>
/// Indicate if a line from a log comes from an old file, or from a file created since log monitoring started.
/// </summary>
type LogData =
    | Old of string
    | Fresh of string
with
    override this.ToString() =
        match this with
        | Old x
        | Fresh x -> x

/// <summary>
/// Return all lines in all existing file logs, and then start monitoring new lines in new files.
/// The last file in the existing file logs is considered to be a new file.
/// </summary>
/// <param name="replayQuick">Function that turns the list of old entries into an AsyncSeq.
/// If the result of this function is meant to be consumed by Async.mergeChoice, one should avoid using a list whose content can be consumed at once.
/// In that case, prefer an asyncSeq that sleeps between each element yield.
/// </param>
/// <param name="path">Path to the directory containing the logs.</param>
/// <param name="filter">Filter identifying log files.</param>
/// <param name="existingFiles">Array of existing files; the last one is considered to be a new file, and its content is returned as fresh.</param>
let resumeWatchlogs(replayQuick, path, filter, existingFiles, cancelToken) =
    let firstToWatch =
        match existingFiles with
        | [||] -> None
        | x -> Some(Array.last x)
    let oldLines =
        [|
            for i in 0 .. existingFiles.Length - 2 do
                yield! File.ReadAllLines(existingFiles.[i])
        |]
    let freshLines =
        watchLogs(false, path, filter, firstToWatch, cancelToken)
    asyncSeq {
        yield! replayQuick oldLines
        for line in freshLines do
            yield Fresh line
    }

/// <summary>
/// A command typed in the chat by a player.
/// </summary>
type Command =
    { Author : string
      Command : string }

/// <summary>
/// Extract commands from the chat.
/// </summary>
/// <param name="path">Path to the directory containing the chatlogs.</param>
let watchCommands(path, cancelToken) =
    let cmdRe = Regex(".*\[\"(.*)\".*\]: !(.*)")
    let lines = watchLogs(false, path, "*.chatlog", None, cancelToken)
    let commands =
        lines
        |> AsyncSeq.choose (fun line ->
            let m = cmdRe.Match(line)
            if m.Success && (string m.Groups.[1] <> "#") then
                Some { Author = string m.Groups.[1]; Command = string m.Groups.[2] }
            else
                None)
    commands