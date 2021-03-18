module Shapes.Line exposing(Line, drawLine,lineBetweenlines, linesBetweenLines, lineCenter, slope)

import Playground exposing(..)
import Shapes.Position as Position exposing(Position)
type alias  Line =
  { pos1 : Position
  , pos2 : Position
  }

drawLine : Color -> Int -> Line -> Shape
drawLine color width line =
  let
    center = lineCenter line  
  in
    rectangle color (toFloat width) (lineSize line)
      |> rotate (lineAngle line)
      |> move (toFloat center.x) (toFloat center.y)

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
        |> toFloat
        |> (*) percent
        |> floor
        |> (+) p2
  in
    Position (posCal line.pos1.x line.pos2.x) (posCal line.pos1.y line.pos2.y)


invertLine : Line -> Line
invertLine line =
  Line 
    (Position.invertPos line.pos1)
    (Position.invertPos line.pos2)

lineAngle : Line -> Float
lineAngle line =
  atan2 (toFloat( line.pos2.x - line.pos1.x )) (toFloat(line.pos1.y - line.pos2.y))
    |> toDegrees

toDegrees : Float -> Float
toDegrees rad =
   rad * 180 / pi
   
lineCenter : Line -> Position
lineCenter line =
  let
    posCenter p1 p2 =
      (p1 + p2) // 2
  in  
    Position (posCenter line.pos1.x line.pos2.x) (posCenter line.pos1.y line.pos2.y) 

slope : Line -> Position
slope line = 
  Position (line.pos1.x - line.pos2.x)  (line.pos1.y - line.pos2.y)

lineSize : Line -> Number
lineSize line =
    let
      sqrDiff p1 p2 =
        (abs (p1 - p2)) ^ 2      
    in 
      (sqrDiff line.pos1.x line.pos2.x) + (sqrDiff line.pos1.y line.pos2.y)
        |> toFloat
        |> sqrt

