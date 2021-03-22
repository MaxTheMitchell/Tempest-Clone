module Main exposing(main)

import Playground exposing (..)

import Levels.Levels as Levels exposing(Levels) 

type alias Memory = 
  {
  levels : Levels
  }

main =
    game view update (Memory Levels.levelsInit)


view : Computer -> Memory -> List(Shape)
view computer memory =
    [
      fillScreen backgroundColor computer.screen
      , (Levels.drawLevels computer.screen memory.levels)
    ]

update : Computer -> Memory -> Memory
update computer memory =
  Memory 
    (Levels.updateLevels computer.keyboard memory.levels)

fillScreen : Color -> Screen -> Shape
fillScreen color screen =
  rectangle color screen.width screen.height

backgroundColor : Color
backgroundColor = black