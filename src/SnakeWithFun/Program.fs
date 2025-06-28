open System
open Spectre.Console
open SnakeWithFun.Model
open SnakeWithFun.Logic
open SnakeWithFun.KeyboardInput

type SpectreConsoleAppState =
    { GameState: GameState
      Canvas: Canvas
      GuiRoot: Layout
      KeyPressHandler: KeyPressHandler }

let initCanvas width height =
    let canvas = Canvas(width + 2, height + 2)

    let borderCoordinates =
        seq {
            for x in 0 .. width + 1 do
                yield (x, 0)
                yield (x, height + 1)

            for y in 0 .. height + 1 do
                yield (0, y)
                yield (width + 1, y)
        }

    Seq.iter (fun (x, y) -> canvas.SetPixel(x, y, Color.White) |> ignore) borderCoordinates

    canvas

let initAppState gameState =
    let canvas = initCanvas gameState.Grid.Width gameState.Grid.Height

    let guiRoot =
        Layout().SplitRows(Layout("Top").MinimumSize(canvas.Height), Layout("Bottom"))

    guiRoot["Top"].Update(canvas) |> ignore

    let keyPressHandler = KeyPressHandler()
    keyPressHandler.StartMonitoring()

    { GameState = gameState
      Canvas = canvas
      GuiRoot = guiRoot
      KeyPressHandler = keyPressHandler }

let updateCanvas appState =
    let { Snake = snake
          Grid = grid
          FoodPosition = foodPosition } =
        appState.GameState

    let canvas = appState.Canvas

    // Clear the grid using sequence expressions
    let gridPositions =
        seq {
            for x in 0 .. grid.Width - 1 do
                for y in 0 .. grid.Height - 1 do
                    yield (x + 1, y + 1)
        }

    gridPositions
    |> Seq.iter (fun (x, y) -> canvas.SetPixel(x, y, Color.Black) |> ignore)

    // Draw the snake
    match snake.Position with
    | head :: body ->
        canvas.SetPixel(head.X + 1, head.Y + 1, Color.Yellow) |> ignore

        body
        |> List.iter (fun pos -> canvas.SetPixel(pos.X + 1, pos.Y + 1, Color.Green) |> ignore)
    | [] -> () // Should never happen in a valid game state

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
    let baseSpeed = 200.0
    let waitTime = baseSpeed * (1.0 / appState.GameState.Speed)

    while (DateTime.Now - startTime).TotalMilliseconds < waitTime do
        Threading.Thread.Yield() |> ignore

    let lastPressedKey =
        if appState.KeyPressHandler.LastPressedKey.IsSome then
            let key = appState.KeyPressHandler.LastPressedKey.Value.Key
            appState.KeyPressHandler.ClearLastKey()
            key
        else
            ConsoleKey.None

    match lastPressedKey with
    | ConsoleKey.W -> DirectionChange Up
    | ConsoleKey.S -> DirectionChange Down
    | ConsoleKey.A -> DirectionChange Left
    | ConsoleKey.D -> DirectionChange Right
    | ConsoleKey.Q -> Quit
    | _ -> Idle


let rec gameLoop appState ctx =

    updateGui appState ctx

    let userInput = readUserInput appState

    let gameEvent = gameTick appState.GameState userInput

    match gameEvent with
    | GameOver -> ()
    | GameQuit -> ()
    | GameContinues newState -> gameLoop { appState with GameState = newState } ctx

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
