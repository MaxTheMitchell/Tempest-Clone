module Shapes.Position exposing(Position, invertPos)

type alias Position =
  { 
    x : Int
  , y : Int
  }

invertPos : Position -> Position
invertPos pos =
  Position pos.y pos.x
