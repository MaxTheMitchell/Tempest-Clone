module Main exposing(main, segments)

import Playground exposing (..)
import Shapes.Rect as Rect
import Shapes.ConnectedRect as CRect
import Shapes.Position exposing(Position)
import Shapes.Line exposing(..)
import Characters.Player as Player
import Array exposing (Array)

type alias Memory =
    { x : Int
    , y : Float
    , count : Int 
    }

main =
  game view update (Memory 0 0 0)


view computer memory =
    testVeiw (Position 0 0) computer.screen memory

update : Computer -> Memory -> Memory
update computer memory =
  if memory.count == 0 then 
    Memory
    (
      toX computer.keyboard
      |> floor
      |> (+) memory.x
      |> (+) (4 * segments) 
      |> modBy (4 * segments)
    )
    (
      toY computer.keyboard
      |> (*) yMove
      |> (+) memory.y
    )
    (
      modBy updateCount (memory.count + 1)
    )
  else
    Memory
      memory.x
      memory.y 
      (
        modBy updateCount (memory.count + 1)
      )


testVeiw : Position -> Screen -> Memory -> List(Shape)
testVeiw pos screen memory=
  let 
    outerSize = (floor (min screen.height screen.width) - 100)
    outerRect = Rect.square pos outerSize
    innerRect = Rect.square pos (floor (toFloat outerSize * 0.1))

    rect = CRect.ConnectedRect outerRect innerRect
  in
    [
      fillScreen backgroundColor screen
      ,CRect.drawConnectedRect blue shapeColor backgroundColor segments lineWidthConst rect
      ,CRect.linesBetweenConnectedPairs (memory.y) segments (CRect.ConnectedRect outerRect innerRect)
       |> Array.fromList
       |> Array.get memory.x
       |> Maybe.withDefault (Line (Position 0 0) (Position 0 0)) 
       |> (Player.drawPlayer yellow lineWidthConst)
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
segments = 4

updateCount : Int
updateCount = 3

yMove : Float
yMove = 0.2