module Shapes.Polygon exposing(..)

import Playground exposing (Shape, Number, Color, polygon)
import Shapes.Line as Line
import Shapes.Position as Position exposing(Position)

type alias Polygon{
    points : List(Position)
}

drawPolygon : Color -> Polygon -> Shape
drawPolygon color poly =
  poly.points
    |> (polygon color)