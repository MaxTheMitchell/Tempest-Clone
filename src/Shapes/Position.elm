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

rotatePos : Float -> Position -> Position
rotatePos angle pos =
  Position
    (pos.x*(cos (degrees angle)) - pos.y*(sin (degrees angle)) )
    (pos.y*(cos (degrees angle)) + pos.x*(sin (degrees angle))) 