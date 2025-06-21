type Position = { X: int; Y: int }
type Snake = { Length: int; Position: Position list }
type Grid = { Width: int; Height: int }
type Direction = Up | Down | Left | Right
type GameState = { Snake: Snake; Grid: Grid; CurrentDirection: Direction }
type UserInput =
    | DirectionChange of Direction
    | Idle
    | Quit
    
let wrappedPositionUpdate (pos: Position) (grid: Grid) =
    let wrappedX = if pos.X < 0 then grid.Width - 1 elif pos.X >= grid.Width then 0 else pos.X
    let wrappedY = if pos.Y < 0 then grid.Height - 1 elif pos.Y >= grid.Height then 0 else pos.Y
    { X = wrappedX; Y = wrappedY }

let moveSnake (snake: Snake) (direction: Direction) (grid : Grid)=
    let head = List.head snake.Position
    let newHead =
        match direction with
        | Up -> { X = head.X; Y = head.Y - 1 }
        | Down -> { X = head.X; Y = head.Y + 1 }
        | Left -> { X = head.X - 1; Y = head.Y }
        | Right -> { X = head.X + 1; Y = head.Y }
    
    let newPosition = newHead :: List.take (snake.Length - 1) snake.Position |> List.map (fun pos -> wrappedPositionUpdate pos grid)
    { snake with Position = newPosition }
    
let gameTick (state: GameState) (input: UserInput) =
    let newDirection =
        match input with
        | DirectionChange dir -> dir
        | Idle -> state.CurrentDirection
        | Quit -> failwith "Game Over"
    
    let newSnake = moveSnake state.Snake newDirection state.Grid
    { state with Snake = newSnake; CurrentDirection = newDirection }

let printState {Snake=snake;Grid=grid} =
    // print border around the grid
    for _ in 0 .. grid.Width + 1 do
        printf "-"
    printfn ""
    
    // print the grid with the snake
    for y in 0 .. grid.Height - 1 do
        printf "|"
        for x in 0 .. grid.Width - 1 do
            if List.exists (fun pos -> pos.X = x && pos.Y = y) snake.Position then
                printf "S"
            else
                printf " "
        printf "|"
        printfn ""
    
    // print border around the grid
    for _ in 0 .. grid.Width + 1 do
        printf "-"
        
let checkKeyPress = System.Windows.Input.Keyboard.IsKeyDown
        
let grid = { Width = 20; Height = 10 }
let initialSnake = { Length = 3; Position = [{ X = 1; Y = 1 }; { X = 1; Y = 2 }; { X = 1; Y = 3 }] }

let initialState = { Snake = initialSnake; Grid = grid; CurrentDirection = Right }

let rec gameLoop state =
    // Delay for a short period to simulate game tick
    System.Threading.Thread.Sleep(250)
    // Clear the console
    System.Console.Clear()
    printState state
    printfn ""
    printfn "Enter direction (w/a/s/d) or 'q' to quit:"
    
    
    let userInput =
        if checkKeyPress System.Windows.Input.Key.W then DirectionChange Up
        elif checkKeyPress System.Windows.Input.Key.S then DirectionChange Down
        elif checkKeyPress System.Windows.Input.Key.A then DirectionChange Left
        elif checkKeyPress System.Windows.Input.Key.D then DirectionChange Right
        elif checkKeyPress System.Windows.Input.Key.Q then Quit
        else Idle
    
    try
        let newState = gameTick state userInput
        if userInput <> Quit then
            gameLoop newState
    with
    | ex -> printfn "Error: %s" ex.Message
    
// we need sta thread here to use WPF input
[<EntryPoint>]
[<System.STAThread>]
let main _ =
  gameLoop initialState
  0