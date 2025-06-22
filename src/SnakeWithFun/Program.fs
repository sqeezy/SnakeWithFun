open System
open System.Windows.Input

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
      FoodPosition: Position }

type UserInput =
    | DirectionChange of Direction
    | Idle
    | Quit

let wrappedPositionUpdate (pos: Position) (grid: Grid) =
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
    match (current, userRequest) with
    | Up, Down
    | Down, Up
    | Left, Right
    | Right, Left -> current // No change if opposite direction requested
    | _, _ -> userRequest // Update to new direction if valid

let findNewFoodPosition (snake: Snake) (grid: Grid) =
    let random = System.Random.Shared

    let gridPositions =
        [ for x in 0 .. grid.Width - 1 do
              for y in 0 .. grid.Height - 1 do
                  yield { X = x; Y = y } ]

    let snakePositions = snake.Position |> Set.ofList

    let availablePositions =
        gridPositions |> List.filter (fun pos -> not (Set.contains pos snakePositions))

    if availablePositions.IsEmpty then
        failwith "you should have won the game already"

    let randomIndex = random.Next(availablePositions.Length)
    availablePositions[randomIndex]

type MoveResult =
    | SnakeMoved of Position
    | SnakeEatenFood of Position
    | Collision

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
    |> List.map (fun pos -> wrappedPositionUpdate pos grid)

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

let gameTick (state: GameState) (input: UserInput) =
    state |> processUserInput input |> previewMove ||> resolveMove

let printState
    { Snake = snake
      Grid = grid
      FoodPosition = foodPosition }
    =
    // print border around the grid
    for _ in 0 .. grid.Width + 1 do
        printf "-"

    printfn ""

    // print the grid with the snake
    for y in 0 .. grid.Height - 1 do
        printf "|"

        for x in 0 .. grid.Width - 1 do
            if List.exists (fun pos -> pos.X = x && pos.Y = y) snake.Position then
                printf "X"
            elif x = snake.Position.Head.X && y = snake.Position.Head.Y then
                printf "F"
            elif x = foodPosition.X && y = foodPosition.Y then
                printf "O"
            else
                printf " "

        printf "|"
        printfn ""

    // print border around the grid
    for _ in 0 .. grid.Width + 1 do
        printf "-"

let checkKeyPress = Keyboard.IsKeyDown

let rec gameLoop state =
    Console.Clear()
    printState state
    printfn ""
    printfn "Enter direction (w/a/s/d) or 'q' to quit:"

    let startTime = DateTime.Now
    let mutable lastPressedKey = ConsoleKey.None
    // while 250 milliseconds have not passed
    while (DateTime.Now - startTime).TotalMilliseconds < 250 do
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
        let newState = gameTick state userInput

        match newState with
        | None -> ()
        | Some newState -> gameLoop newState
    with ex ->
        printfn "Error: %s" ex.Message

let grid = { Width = 20; Height = 20 }

let initialSnake =
    { Position = [ { X = 1; Y = 1 }; { X = 1; Y = 2 }; { X = 1; Y = 3 } ] }

let initialState =
    { Snake = initialSnake
      Grid = grid
      CurrentDirection = Right
      FoodPosition = { X = 2; Y = 2 } }

// we need sta thread here to use WPF input
[<EntryPoint>]
[<System.STAThread>]
let main _ =
    gameLoop initialState
    0
