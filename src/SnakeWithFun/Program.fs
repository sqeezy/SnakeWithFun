open System
open Spectre.Console

type Position = { X: int; Y: int }
type Snake = { Position: Position list }
type Grid = { Width: int; Height: int }

type Direction =
    | Up
    | Down
    | Left
    | Right

type GameState =
    { Snake: Snake
      Grid: Grid
      CurrentDirection: Direction
      FoodPosition: Position
      Speed: float }

type UserInput =
    | DirectionChange of Direction
    | Idle
    | Quit

type MoveResult =
    | SnakeMoved of Position
    | SnakeEatenFood of Position
    | Collision

let wrappedPositionUpdate (grid: Grid) (pos: Position) =
    let wrappedX =
        if pos.X < 0 then grid.Width - 1
        elif pos.X >= grid.Width then 0
        else pos.X

    let wrappedY =
        if pos.Y < 0 then grid.Height - 1
        elif pos.Y >= grid.Height then 0
        else pos.Y

    { X = wrappedX; Y = wrappedY }

let updatedDirection (current: Direction) (userRequest: Direction) =
    match current, userRequest with
    | Up, Down
    | Down, Up
    | Left, Right
    | Right, Left -> current // No change if opposite direction requested
    | _ -> userRequest // Update to new direction if valid

let findNewFoodPosition (snake: Snake) (grid: Grid) =
    let random = Random.Shared

    let gridPositions =
        [ for x in 0 .. grid.Width - 1 do
              for y in 0 .. grid.Height - 1 do
                  yield { X = x; Y = y } ]

    let snakePositions = snake.Position |> Set.ofList

    let availablePositions =
        gridPositions |> List.filter (fun pos -> not (Set.contains pos snakePositions))

    if availablePositions.IsEmpty then
        failwith "you should have won the game already"

    let randomIndex = random.Next availablePositions.Length
    availablePositions[randomIndex]

let calculateMoveResult (state: GameState) (newHead: Position) =
    let snake = state.Snake

    // Check if the new head position collides with the snake's body
    if List.exists (fun pos -> pos = newHead) (snake.Position |> List.take (snake.Position.Length - 1)) then
        Collision
    else if newHead = state.FoodPosition then
        SnakeEatenFood newHead
    else
        SnakeMoved newHead

let updatePositions snake grid nextHead foodWasEaten =
    let eatingManipulation = if foodWasEaten then 0 else 1

    nextHead
    :: List.take (snake.Position.Length - eatingManipulation) snake.Position
    |> List.map (wrappedPositionUpdate grid)

let processUserInput userInput state =
    let newDirection =
        match userInput with
        | DirectionChange dir -> updatedDirection state.CurrentDirection dir
        | Idle -> state.CurrentDirection
        | Quit -> failwith "Game Over"

    { state with
        CurrentDirection = newDirection }

let previewMove state =
    let snake = state.Snake
    let head = List.head snake.Position

    let nextHead =
        match state.CurrentDirection with
        | Up -> { X = head.X; Y = head.Y - 1 }
        | Down -> { X = head.X; Y = head.Y + 1 }
        | Left -> { X = head.X - 1; Y = head.Y }
        | Right -> { X = head.X + 1; Y = head.Y }
        |> wrappedPositionUpdate state.Grid

    state, nextHead

let resolveMove state nextHead =
    let moveResult = calculateMoveResult state nextHead

    match moveResult with
    | Collision -> None
    | SnakeMoved nextHead ->
        let newPosition = updatePositions state.Snake state.Grid nextHead false

        let newSnake =
            { state.Snake with
                Position = newPosition }

        Some { state with Snake = newSnake }
    | SnakeEatenFood nextHead ->
        let newPosition = updatePositions state.Snake state.Grid nextHead true

        let newSnake =
            { state.Snake with
                Position = newPosition }

        Some
            { state with
                Snake = newSnake
                FoodPosition = findNewFoodPosition newSnake state.Grid }

let updateSpeed state =

    let speedFactor state =
        match state.Snake.Position.Length with
        | n when n < 5 -> 1.0
        | n when n < 10 -> 2.0
        | _ -> 3.0

    let speedFactor = speedFactor state
    { state with Speed = speedFactor }


let gameTick (state: GameState) (input: UserInput) =
    state |> updateSpeed |> processUserInput input |> previewMove ||> resolveMove

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
        Layout()
            .SplitRows(
                Layout("Top").MinimumSize(canvas.Height),
                Layout("Bottom"))

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

let updateDataPane appState  =
    appState.GuiRoot["Bottom"].Update(
        Panel(
            Table()
                .AddColumn("Property")
                .AddColumn("Value")
                .AddRow("Snake Length", string (List.length appState.GameState.Snake.Position))
                .AddRow("Food Position", sprintf "(%d, %d)" appState.GameState.FoodPosition.X appState.GameState.FoodPosition.Y)
                .AddRow("Speed", string appState.GameState.Speed)
        )
    ) |> ignore

let updateGui appState (ctx: LiveDisplayContext) =
    updateCanvas appState
    updateDataPane appState

    ctx.Refresh()

let rec gameLoop appState ctx =

    updateGui appState ctx

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

    try
        let newState = gameTick appState.GameState userInput

        match newState with
        | None -> ()
        | Some newState -> gameLoop { appState with GameState = newState } ctx
    with ex ->
        printfn "Error: %s\r\n%s" ex.Message ex.StackTrace

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
    AnsiConsole.Live(appState.GuiRoot).Start(fun ctx -> gameLoop appState ctx)

    0
