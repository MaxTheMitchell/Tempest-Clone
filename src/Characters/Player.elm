module Characters.Player exposing(..)

import Playground exposing(Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)

drawPlayer : Color -> Number -> Line -> Shape
drawPlayer color size line =
  let 
    slope = Line.slope line
    center = Position 
      ((Line.lineCenter line).x - (floor ((toFloat slope.y ) * playerSize)))
      ((Line.lineCenter line).y - (floor ((toFloat slope.x ) * playerSize)))
  in
    Playground.group 
      [
        Line.drawLine color size (Line line.pos1 center)
        ,Line.drawLine color size (Line line.pos2 center)
      ]

playerSize : Float
playerSize = 0.2