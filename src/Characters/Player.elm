module Characters.Player exposing(..)

import Playground exposing(Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.ConnectedRect as CRect exposing(ConnectedRect)
import Shapes.Position as Position exposing(Position)
import Array exposing (Array)

type alias Player =
    {x : Int
    , y : Float
    }

drawPlayer : Color -> Number -> ConnectedRect -> Player -> Shape
drawPlayer color size cRect player =
  let 
    line = playerLine cRect player
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

playerLine : ConnectedRect -> Player -> Line
playerLine cRect player =
  cRect
    |> (CRect.linesBetweenConnectedPairs player.y)
    |> Array.fromList
    |> Array.get player.x
    |> Maybe.withDefault (Line (Position 0 0) (Position 0 0)) 

playerSize : Float
playerSize = 0.2