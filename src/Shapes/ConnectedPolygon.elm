module Shapes.ConnectedPolygon exposing(..)

import Playground exposing (Shape, Number, Color)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)
import Shapes.Polygon as Poly exposing(Polygon)
type alias ConnectedPolygon = 
  {
  segments : Int
  ,innerPoly : Polygon
  ,outerPoly : Polygon
  }

drawConnectedPoly : Playground.Screen -> Color -> Int -> ConnectedPolygon -> Shape
drawConnectedPoly screen color lineWidth cPoly =
   [
    Poly.drawPoly screen color lineWidth cPoly.outerPoly
    ,Poly.drawPoly screen color lineWidth cPoly.innerPoly
  ] ++ drawLinesBetween screen color lineWidth cPoly
    |> Playground.group


drawLinesBetween : Playground.Screen -> Color -> Int -> ConnectedPolygon -> List(Shape)
drawLinesBetween screen color lineWidth cPoly =
  List.map (Line.drawLine screen color lineWidth) 
    (linesBetween cPoly)

grow : Float -> ConnectedPolygon -> ConnectedPolygon
grow percent cPoly =
  ConnectedPolygon
    cPoly.segments
    (Poly.grow percent cPoly.innerPoly)
    (Poly.grow percent cPoly.outerPoly)

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
  (List.map2
    Line
    (Poly.positionsOnPeremeter cPoly.segments cPoly.innerPoly)
    (Poly.positionsOnPeremeter cPoly.segments cPoly.outerPoly))