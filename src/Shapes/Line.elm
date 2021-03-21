module Shapes.Line exposing(..)

import Playground exposing(..)
import Shapes.Position as Position exposing(Position)
type alias  Line =
  { pos1 : Position
  , pos2 : Position
  }

drawLine : Playground.Screen -> Color -> Int -> Line -> Shape
drawLine screen color width line =
  let
    center = Position.toScreenPos screen (lineCenter line)  
  in
    rectangle color (toFloat width) ((lineSize line) * (min screen.width screen.height))
      |> rotate (lineAngle line)
      |> move center.x center.y

linesBetweenLines : Int -> Line -> Line -> List(Line)
linesBetweenLines amount line1 line2 =
  List.map2 (Line) (positionsOnLine amount line1) (positionsOnLine amount line2)

positionsOnLine : Int -> Line -> List(Position)
positionsOnLine amount line =
  let 
    percent = 1 / (toFloat amount)
  in
    List.range 1 amount
      |> List.map toFloat
      |> List.map (\i -> positionOnLine (i*percent) line)

lineBetweenlines : Float -> Line -> Line -> Line
lineBetweenlines percent line1 line2 = 
  Line
    (positionOnLine percent line1)
    (positionOnLine percent line2)  

positionOnLine : Float -> Line -> Position
positionOnLine percent line =
  let
    posCal p1 p2 =
      p1 - p2
        |> (*) percent
        |> (+) p2
  in
    Position (posCal line.pos1.x line.pos2.x) (posCal line.pos1.y line.pos2.y)

lineAngle : Line -> Float
lineAngle line =
  atan2 (line.pos2.x - line.pos1.x ) (line.pos1.y - line.pos2.y)
    |> toDegrees

toDegrees : Float -> Float
toDegrees rad =
   rad * 180 / pi
   
lineCenter : Line -> Position
lineCenter line =
  let
    posCenter p1 p2 =
      (p1 + p2) / 2
  in  
    Position (posCenter line.pos1.x line.pos2.x) (posCenter line.pos1.y line.pos2.y) 

slope : Line -> Position
slope line = 
  Position (line.pos1.x - line.pos2.x)  (line.pos1.y - line.pos2.y)

sumLines : List(Line) -> Float 
sumLines list =
  list
    |> List.map lineSize
    |> List.sum 

lineSize : Line -> Float
lineSize line =
    let
      sqrDiff p1 p2 =
        (abs (p1 - p2)) ^ 2      
    in 
      (sqrDiff line.pos1.x line.pos2.x) + (sqrDiff line.pos1.y line.pos2.y)
        |> sqrt

