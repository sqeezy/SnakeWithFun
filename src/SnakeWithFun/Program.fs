open System
open Spectre.Console
open SnakeWithFun.Model
open SnakeWithFun.Logic

type SpectreConsoleAppState =
    { GameState: GameState
      Canvas: Canvas
      GuiRoot: Layout }

let initCanvas width height =
    let canvas = Canvas(width + 2, height + 2)

    for x in 0 .. canvas.Height - 1 do
        canvas.SetPixel(0, x, Color.White).SetPixel(canvas.Width - 1, x, Color.White)
        |> ignore

    for y in 0 .. canvas.Width - 1 do
        canvas.SetPixel(y, 0, Color.White).SetPixel(y, canvas.Height - 1, Color.White)
        |> ignore

    canvas

let initAppState gameState =
    let canvas = initCanvas gameState.Grid.Width gameState.Grid.Height

    let guiRoot =
        Layout().SplitRows(Layout("Top").MinimumSize(canvas.Height), Layout("Bottom"))

    guiRoot["Top"].Update(canvas) |> ignore

    { GameState = gameState
      Canvas = canvas
      GuiRoot = guiRoot }

let updateCanvas appState =
    let { Snake = snake
          Grid = grid
          FoodPosition = foodPosition } =
        appState.GameState

    let canvas = appState.Canvas

    for x in 0 .. grid.Width - 1 do
        for y in 0 .. grid.Height - 1 do
            canvas.SetPixel(x + 1, y + 1, Color.Black) |> ignore

    // Draw the snake
    let head :: body = snake.Position
    canvas.SetPixel(head.X + 1, head.Y + 1, Color.Yellow) |> ignore

    for pos in body do
        canvas.SetPixel(pos.X + 1, pos.Y + 1, Color.Green) |> ignore

    // Draw the food
    canvas.SetPixel(foodPosition.X + 1, foodPosition.Y + 1, Color.Red) |> ignore

let updateDataPane appState =
    appState.GuiRoot["Bottom"]
        .Update(
            Panel(
                Table()
                    .AddColumn("Property")
                    .AddColumn("Value")
                    .AddRow("Snake Length", string (List.length appState.GameState.Snake.Position))
                    .AddRow(
                        "Food Position",
                        sprintf "(%d, %d)" appState.GameState.FoodPosition.X appState.GameState.FoodPosition.Y
                    )
                    .AddRow("Speed", string appState.GameState.Speed)
            )
        )
    |> ignore

let updateGui appState (ctx: LiveDisplayContext) =
    updateCanvas appState
    updateDataPane appState

    ctx.Refresh()

let readUserInput appState =
    let startTime = DateTime.Now
    let mutable lastPressedKey = ConsoleKey.None
    let baseSpeed = 200.0
    let waitTime = baseSpeed * (1.0 / appState.GameState.Speed)

    while (DateTime.Now - startTime).TotalMilliseconds < waitTime do
        System.Threading.Thread.Sleep(1)

        if Console.KeyAvailable then
            lastPressedKey <- Console.ReadKey(true).Key

    let userInput =
        match lastPressedKey with
        | ConsoleKey.W -> DirectionChange Up
        | ConsoleKey.S -> DirectionChange Down
        | ConsoleKey.A -> DirectionChange Left
        | ConsoleKey.D -> DirectionChange Right
        | ConsoleKey.Q -> Quit
        | _ -> Idle

    userInput


let rec gameLoop appState ctx =

    updateGui appState ctx

    let userInput = readUserInput appState

    let newState = gameTick appState.GameState userInput

    match newState with
    | None -> ()
    | Some newState -> gameLoop { appState with GameState = newState } ctx

let grid = { Width = 20; Height = 20 }

let initialSnake =
    { Position = [ { X = 1; Y = 1 }; { X = 1; Y = 2 }; { X = 1; Y = 3 } ] }

let initialState =
    { Snake = initialSnake
      Grid = grid
      CurrentDirection = Right
      FoodPosition = findNewFoodPosition initialSnake grid
      Speed = 1.0 }

// we need sta thread here to use WPF input
[<EntryPoint>]
[<STAThread>]
let main _ =
    let appState = initAppState initialState

    AnsiConsole
        .Live(appState.GuiRoot)
        .Start(fun ctx ->
            try
                gameLoop appState ctx
            with ex ->
                printfn "Error: %s\r\n%s" ex.Message ex.StackTrace)

    0
