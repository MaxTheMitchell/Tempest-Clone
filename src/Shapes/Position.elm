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
    (pos.x* ((min screen.width screen.height)))
    (pos.y* ((min screen.width screen.height)))