module Shapes.Polygon exposing(..)

import Playground exposing (Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)
import Array

type alias Polygon = List(Position)


drawPoly : Color -> Int -> Polygon -> Shape
drawPoly color lineWidth poly =
  toLines poly
    |> List.map (Line.drawLine color lineWidth)
    |> Playground.group

toLines : Polygon -> List(Line)
toLines poly = 
    [Maybe.withDefault (Position 0 0) (List.head poly)]
      |> (++) (Maybe.withDefault [] (List.tail poly))
      |> (\l -> List.map2 Line l poly)
   
getCorner : Int -> Polygon -> Position
getCorner i poly =
  poly 
    |> Array.fromList 
    |> Array.get i
    |> Maybe.withDefault (Position 0 0)

rect : Position -> Int -> Int -> Polygon
rect pos width height = 
  let 
    halfWidth = width//2
    halfHeight = height//2
  in
    [
      Position (pos.x - halfWidth) (pos.y - halfHeight)
      ,Position (pos.x + halfWidth) (pos.y - halfHeight)
      ,Position (pos.x + halfWidth) (pos.y + halfHeight)
      ,Position (pos.x - halfWidth) (pos.y + halfHeight)
    ]

square : Position -> Int -> Polygon
square pos size =
  rect pos size size