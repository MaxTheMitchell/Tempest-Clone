module Shapes.ConnectedPolygon exposing(..)

import Playground exposing (Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)
import Shapes.Polygon as Poly exposing(Polygon)
import Html.Attributes exposing (list)
import Array

type alias ConnectedPolygon = 
  {
  segments : Int
  ,innerPoly : Polygon
  ,outerPoly : Polygon
  }

drawConnectedPoly : Color -> Int -> ConnectedPolygon -> Shape
drawConnectedPoly color lineWidth cPoly =
   [
    Poly.drawPoly color lineWidth cPoly.outerPoly
    ,Poly.drawPoly color lineWidth cPoly.innerPoly
  ] ++ drawLinesBetween color lineWidth cPoly
    |> Playground.group


drawLinesBetween : Color -> Int -> ConnectedPolygon -> List(Shape)
drawLinesBetween color lineWidth cPoly =
  List.map (Line.drawLine color lineWidth) 
    (linesBetween cPoly)

linesBetweenConnectedPairs : Float -> ConnectedPolygon -> List(Line)
linesBetweenConnectedPairs percent cPoly =
  List.map 
    (\(a,b) -> Line.lineBetweenlines percent a b) 
    (connectedLinesPairs cPoly)

connectedLinesPairs : ConnectedPolygon -> List(Line, Line)
connectedLinesPairs cPoly =
  let 
    lines = linesBetween cPoly
  in
    [Maybe.withDefault (Line (Position 0 0) (Position 0 0)) (List.head lines)]
      |> List.append (Maybe.withDefault [] (List.tail lines))
      |> List.map2 Tuple.pair lines

linesBetween : ConnectedPolygon -> List(Line)
linesBetween cPoly =
  List.map2 
    (Line.linesBetweenLines cPoly.segments) 
    (Poly.toLines cPoly.innerPoly) 
    (Poly.toLines cPoly.outerPoly)
   |> List.concat

-- splitList : Int -> Int List(Line, Line) -> List(Line, Line)
-- splitList list =
 