module Main exposing(main, segments)

import Playground exposing (..)
import Shapes.Rect as Rect
import Shapes.ConnectedRect as CRect
import Shapes.Position exposing(Position)
import Shapes.Line exposing(..)
import Characters.Player as Player exposing(Player)
import Array exposing (Array)

type alias Memory =
    { player : Player
    , count : Int 
    }

main =
  game view update (Memory (Player 0 0) 0)


view computer memory =
    testVeiw (Position 0 0) computer.screen memory

update : Computer -> Memory -> Memory
update computer memory =
  Memory
  (
    if memory.count == 0 then 
      Player
        (
          toX computer.keyboard
          |> floor
          |> (+) memory.player.x
          |> (+) (4 * segments) 
          |> modBy (4 * segments)
        )
        (
          toY computer.keyboard
          |> (*) yMove
          |> (+) memory.player.y
        )
    else
      memory.player
  )
  (
    modBy updateCount (memory.count + 1)
  )


testVeiw : Position -> Screen -> Memory -> List(Shape)
testVeiw pos screen memory=
  let 
    outerSize = (floor (min screen.height screen.width) - 100)
    outerRect = Rect.square pos outerSize
    innerRect = Rect.square pos (floor (toFloat outerSize * 0.1))

    rect = CRect.ConnectedRect outerRect innerRect segments
  in
    [
      fillScreen backgroundColor screen
      ,CRect.drawConnectedRect shapeColor backgroundColor backgroundColor lineWidthConst rect
      ,Player.drawPlayer yellow lineWidthConst rect memory.player
    ]

fillScreen : Color -> Screen -> Shape
fillScreen color screen =
  Rect.drawRect color (Rect.Rect (Position 0 0) (floor screen.width) (floor screen.height))

lineWidthConst : Number
lineWidthConst = 3

shapeColor : Color
shapeColor = red
backgroundColor : Color
backgroundColor = black

segments : Int
segments = 3

updateCount : Int
updateCount = 3

yMove : Float
yMove = 0.1