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
    if List.exists (fun pos -> pos = newHead) snake.Position then
        Collision
    else if newHead = state.FoodPosition then
        SnakeEatenFood newHead
    else
        SnakeMoved newHead

let gameTick (state: GameState) (input: UserInput) =
    let newDirection =
        match input with
        | DirectionChange dir -> updatedDirection state.CurrentDirection dir
        | Idle -> state.CurrentDirection
        | Quit -> failwith "Game Over"

    let snake = state.Snake
    let head = List.head snake.Position

    let nextHead =
        match newDirection with
        | Up -> { X = head.X; Y = head.Y - 1 }
        | Down -> { X = head.X; Y = head.Y + 1 }
        | Left -> { X = head.X - 1; Y = head.Y }
        | Right -> { X = head.X + 1; Y = head.Y }

    let moveResult = calculateMoveResult state nextHead

    let updatePositions snake grid nextHead foodWasEaten =
        let eatingManipulation = if foodWasEaten then 0 else 1

        nextHead
        :: List.take (snake.Position.Length - eatingManipulation) snake.Position
        |> List.map (fun pos -> wrappedPositionUpdate pos grid)

    match moveResult with
    | Collision -> None
    | SnakeMoved nextHead ->
        let newPosition = updatePositions snake state.Grid nextHead false
        let newSnake = { snake with Position = newPosition }

        { state with
            Snake = newSnake
            CurrentDirection = newDirection }
        |> Some
    | SnakeEatenFood nextHead ->
        let newPosition = updatePositions snake state.Grid nextHead true
        let newSnake = { snake with Position = newPosition }

        { state with
            Snake = newSnake
            CurrentDirection = newDirection
            FoodPosition = findNewFoodPosition newSnake state.Grid }
        |> Some

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
    let mutable lastPressedKey  = ConsoleKey.None
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

let grid = { Width = 20; Height = 10 }

let initialSnake =
    { Position = [ { X = 1; Y = 1 }; { X = 1; Y = 2 }; { X = 1; Y = 3 } ] }

let initialState =
    { Snake = initialSnake
      Grid = grid
      CurrentDirection = Right
      FoodPosition = { X = 5; Y = 5 } }

// we need sta thread here to use WPF input
[<EntryPoint>]
[<System.STAThread>]
let main _ =
    gameLoop initialState
    0
