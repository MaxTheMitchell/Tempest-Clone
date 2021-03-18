module Characters.Player exposing(..)

import Playground exposing(Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Position as Position exposing(Position)
import Array exposing (Array)

type alias Player =
    {x : Int
    , y : Float
    }

drawPlayer : Color -> Int -> ConnectedPolygon -> Player -> Shape
drawPlayer color size cPoly player =
  let 
    line = playerLine cPoly player
    slope = Line.slope line
    center = Position 
      ((Line.lineCenter line).x - (floor ((toFloat slope.y ) * playerSize * -1))) --this negitve one fixes the "arrow" pointing for the player. Need to look into more at some point
      ((Line.lineCenter line).y - (floor ((toFloat slope.x ) * playerSize)))
  in
    Playground.group 
      [
        Line.drawLine color size (Line line.pos1 center)
        ,Line.drawLine color size (Line line.pos2 center)
      ]

playerLine : ConnectedPolygon -> Player -> Line
playerLine cPoly player =
  cPoly
    |> (CPoly.linesBetweenConnectedPairs player.y)
    |> Array.fromList
    |> Array.get player.x
    |> Maybe.withDefault (Line (Position 0 0) (Position 0 0)) 

playerSize : Float
playerSize = 0.2