module Shapes.Polygon exposing(..)

import Playground exposing (Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)
import Array

type alias Polygon = List(Position)


drawPoly : Playground.Screen -> Color -> Int -> Polygon -> Shape
drawPoly screen color lineWidth poly =
  poly 
    |> toLines
    |> List.map (Line.drawLine screen color lineWidth)
    |> Playground.group

grow : Float -> Position -> Polygon  -> Polygon
grow percent orgin poly =
  List.map (Position.grow percent orgin) poly

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
  List.range 0 (amount - 1)
    |> List.map (\i -> posOnPeremeter 
      ((1 / toFloat amount)*(toFloat i)) poly )

posOnPeremeter : Float -> Polygon -> Position
posOnPeremeter percent poly =
  let
    realFunc perc lines = 
      let 
        len = lines
          |> Line.sumLines
          |> (*) perc
        tail = (Maybe.withDefault [] (List.tail lines))
      in 
        case List.head lines of 
          Nothing -> (Position 0 0)
          Just head ->
            if (Line.lineSize head) >= len then 
              Line.positionOnLine 
                (perc * (1 / ((Line.lineSize head) / (Line.sumLines lines))))
                head
            else 
              realFunc 
                ((len - (Line.lineSize head)) /(Line.sumLines tail) )
                tail
  in
    poly
      |> toLines
      |> List.filter (\line -> Line.lineSize line /= 0) 
      |> (realFunc percent)

peremeter : Polygon -> Float
peremeter poly =
 poly
  |> toLines
  |> Line.sumLines

rect : Position -> Float -> Float -> Polygon
rect pos width height = 
    [
      Position (pos.x - width) (pos.y - height)
      ,Position (pos.x + width) (pos.y - height)
      ,Position (pos.x + width) (pos.y + height)
      ,Position (pos.x - width) (pos.y + height)
    ]

equaladeral : Int -> Position -> Float -> Polygon
equaladeral sides center size = 
  let
    angle = 180 - ((toFloat(sides - 2) * 180) / (toFloat sides))
    point = Position 0 1
  in
    List.range 0 (sides - 1)
      |> List.map (\i -> Position.rotatePos (angle*(toFloat i)) point)
      |> List.map (\pos -> Position (pos.x*size + center.x) (pos.y*size + center.y)) 

starShape : Int -> Position -> Float -> Polygon
starShape points center size = 
  let
    rotatePoint = Position (center.x + size) center.y 
    smallerRotatePoint = Line.lineCenter (Line center rotatePoint)
  in
  List.range 0 points
    |> List.map (\i ->
      Position.rotateAroundPoint ((360/toFloat points)*toFloat i) center 
      (if modBy 2 i == 0 then rotatePoint else smallerRotatePoint)  
      )