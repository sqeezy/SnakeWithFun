module SnakeWithFun.KeyboardInput

open System
open System.Threading
open System.Threading.Tasks

type KeyPressHandler() =
    let mutable isRunning = true
    let mutable keyMonitorTask: Task option = None
    let mutable lastPressedKey: ConsoleKeyInfo option = None
    let lockObj = obj ()

    member this.LastPressedKey = lock lockObj (fun () -> lastPressedKey)

    member this.StartMonitoring() =
        keyMonitorTask <-
            Some(
                Task.Run(fun () ->
                    while isRunning do
                        if Console.KeyAvailable then
                            let keyInfo = Console.ReadKey(true)
                            lock lockObj (fun () -> lastPressedKey <- Some keyInfo)

                        Thread.Sleep(1))
            )

    member this.StopMonitoring() =
        isRunning <- false

        match keyMonitorTask with
        | Some task -> task.Wait()
        | None -> ()

    member this.ClearLastKey() =
        lock lockObj (fun () -> lastPressedKey <- None)
