module Shapes.Position exposing(..)
import Playground
type alias Position =
  { 
    x : Float
  , y : Float
  }

toScreenPos : Playground.Screen -> Position -> Position
toScreenPos screen pos =
  Position
    (pos.x * ((min screen.width screen.height)/2))
    (pos.y * ((min screen.width screen.height)/2))

rotateAroundPoint : Float -> Position -> Position -> Position
rotateAroundPoint angle orgin pos =
  let 
    point = rotatePos angle (Position (pos.x - orgin.x) (pos.y - orgin.y))
  in 
    Position (point.x + orgin.x) (point.y + orgin.y)

rotatePos : Float -> Position -> Position
rotatePos angle pos =
  Position
    (pos.x*(cos (degrees angle)) - pos.y*(sin (degrees angle)) )
    (pos.y*(cos (degrees angle)) + pos.x*(sin (degrees angle))) 