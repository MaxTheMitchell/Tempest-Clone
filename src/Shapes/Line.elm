module Shapes.Line exposing(Line, drawLine, linesBetweenLines)

import Playground exposing(..)
import Shapes.Position exposing(Position)
import Html exposing (li)

type alias  Line =
  { pos1 : Position
  , pos2 : Position
  }

drawLine : Color -> Number -> Line -> Shape
drawLine color width line =
  let
    center = lineCenter line  
  in
    rectangle color width (lineSize line)
      |> rotate (lineAngle line)
      |> move (toFloat center.x) (toFloat center.y)

linesBetweenLines : Int -> Line -> Line -> List(Line)
linesBetweenLines amount line1 line2 =
  List.map2 (Line) (positionsOnLine amount line1) (positionsOnLine amount line2)

positionsOnLine : Int -> Line -> List(Position)
positionsOnLine amount line =
  let 
    spacing p1 p2 =
      (p1 - p2)//amount
        |> abs 
  in
    List.range 0 amount
      |> List.map (\i -> Position 
        (i*(spacing line.pos1.x line.pos2.x) + (min line.pos1.x line.pos2.x)) 
        (i*(spacing line.pos1.y line.pos2.y) + (min line.pos1.y line.pos2.y))
      )

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

lineSize : Line -> Number
lineSize line =
    let
      sqrDiff p1 p2 =
        (abs (p1 - p2)) ^ 2      
    in 
      (sqrDiff line.pos1.x line.pos2.x) + (sqrDiff line.pos1.y line.pos2.y)
        |> toFloat
        |> sqrt

