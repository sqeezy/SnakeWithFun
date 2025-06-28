// basic module setup
module SnakeWithFun.Model

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

type GameEvent =
    | GameContinues of GameState
    | GameOver
    | GameQuit

