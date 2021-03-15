module Main exposing(main, segments)

import Playground exposing (..)
import Shapes.Rect as Rect
import Shapes.ConnectedRect as CRect
import Shapes.Position exposing(Position)
import Shapes.Line exposing(..)
import Characters.Player as Player
import Array exposing (Array)

main =
  game view update 0


view computer offset =
    testVeiw (Position 0 0) computer.screen (floor offset)

update computer offset =
  if floor offset > segments * 4 then
    0
  else
    offset + 0.1


testVeiw : Position -> Screen -> Int -> List(Shape)
testVeiw pos screen offset=
  let 
    outerSize = (floor (min screen.height screen.width) - 100)
    outerRect = Rect.square pos outerSize
    innerRect = Rect.square pos (floor (toFloat outerSize * 0.1))

    rect = CRect.ConnectedRect outerRect innerRect
  in
    [
      fillScreen backgroundColor screen
      ,CRect.drawConnectedRect blue shapeColor backgroundColor segments lineWidthConst rect
    ] 
    ++
    (
      CRect.linesBetweenConnectedPairs 0 segments (CRect.ConnectedRect outerRect innerRect)
       |> Array.fromList
       |> Array.slice offset (offset + 1)
       |> Array.toList
       |> List.map (Player.drawPlayer yellow lineWidthConst)
  )

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
segments = 10