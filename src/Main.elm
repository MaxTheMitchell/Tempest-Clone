module Main exposing(main)

import Playground exposing (..)
import Shapes.Rect as Rect
import Shapes.Position exposing(Position)
import Shapes.Line exposing(..)
import Playground exposing (Color)

main =
  game view update 0


view computer offset =
    testVeiw (Position 0 0) computer.screen

update computer offset =
  offset + 0

testVeiw : Position -> Screen -> List(Shape)
testVeiw pos screen=
  let 
    outerSize = (floor (min screen.height screen.width) - 100)
    outerRect = Rect.square pos outerSize
    innerRect = Rect.square pos (floor (toFloat outerSize * 0.1))
  in
    [
      fillScreen backgroundColor screen
      ,Rect.drawConnectedRect blue shapeColor backgroundColor 4 lineWidthConst innerRect outerRect
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
