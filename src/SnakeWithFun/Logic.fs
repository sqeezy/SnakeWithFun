module SnakeWithFun.Logic

open SnakeWithFun.Model

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
