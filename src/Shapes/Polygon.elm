module Shapes.Polygon exposing(..)

import Playground exposing (Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)
import Array
import Array exposing (length)

type alias Polygon = List(Position)


drawPoly : Color -> Int -> Polygon -> Shape
drawPoly color lineWidth poly =
  poly 
    |> toLines
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

positionsOnPeremeter : Int -> Polygon -> List(Position)
positionsOnPeremeter amount poly =
  List.range 0 amount
    |> List.map (\i -> posOnPeremeter 
      ((1 / toFloat amount)*(toFloat i)) poly )

posOnPeremeter : Float -> Polygon -> Position
posOnPeremeter percent poly =
  let
    realFunc perc lines = 
      let 
        len = lines
          |> Line.sumLines
          |> toFloat
          |> (*) perc
          |> truncate
        tail = (Maybe.withDefault [] (List.tail lines))
      in 
        case List.head lines of 
        Nothing -> (Position 0 0)
        Just head ->
          if (Line.lineSize head) >= len then 
            Line.positionOnLine 
              (perc * (1 / ((toFloat(Line.lineSize head)) / toFloat(Line.sumLines lines))))
              head
          else 
            realFunc 
              (toFloat (len - (Line.lineSize head)) /  toFloat(Line.sumLines tail) )
              tail
  in
    poly
      |> toLines
      |> (realFunc percent)

peremeter : Polygon -> Int
peremeter poly =
 poly
  |> toLines
  |> Line.sumLines

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