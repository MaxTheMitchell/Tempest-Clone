module Shapes.ConnectedRect exposing (..)

import Playground exposing(..)
import Shapes.Position exposing(Position)
import Shapes.Line as Line exposing(Line)
import Shapes.Rect as Rect
import Array


type alias ConnectedRect = 
    {
    outerRect : Rect.Rect
    ,innerRect : Rect.Rect
    }


drawConnectedRect : Color -> Color -> Color -> Int -> Number  -> ConnectedRect -> Shape
drawConnectedRect color fillColor backgroundColor amount lineWidth cRect =
  [
    Rect.drawRect fillColor cRect.outerRect
    ,Rect.drawRect backgroundColor cRect.innerRect
    ,Rect.drawEmptyRect color lineWidth cRect.outerRect
    ,Rect.drawEmptyRect color lineWidth cRect.innerRect
  ] ++ drawLinesBetweenRects color amount lineWidth cRect
    |> group

drawLinesBetweenRects : Color -> Int -> Number -> ConnectedRect -> List(Shape)
drawLinesBetweenRects color amount lineWidth cRect =
  List.map (Line.drawLine color lineWidth) 
    (linesBetweenRects amount cRect)


linesBetweenConnectedPairs : Float -> Int -> ConnectedRect -> List(Line)
linesBetweenConnectedPairs percent amount cRect =
  List.map 
    (\(a,b) -> Line.lineBetweenlines percent a b) 
    (connectedLinesPairs amount cRect)

connectedLinesPairs : Int -> ConnectedRect -> List(Line, Line)
connectedLinesPairs amount cRect =
  let 
    lines = linesBetweenRects amount cRect
  in
    [Maybe.withDefault (Line (Position 0 0) (Position 0 0)) (List.head lines)]
      |> List.append (Maybe.withDefault [] (List.tail lines))
      |> List.map2 Tuple.pair lines

linesBetweenRects : Int -> ConnectedRect -> List(Line)
linesBetweenRects amount cRect =
  List.map2 
      (Line.linesBetweenLines amount) 
      (Rect.edges cRect.innerRect) 
      (Rect.edges cRect.outerRect)
    |> List.concat