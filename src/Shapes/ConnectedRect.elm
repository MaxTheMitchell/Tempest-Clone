module Shapes.ConnectedRect exposing (..)

import Playground exposing(..)
import Shapes.Position exposing(Position)
import Shapes.Line as Line exposing(Line)
import Shapes.Rect as Rect

type alias ConnectedRect = 
    {
    outerRect : Rect.Rect
    ,innerRect : Rect.Rect
    , segments : Int 
    }


drawConnectedRect : Color -> Color -> Color -> Number  -> ConnectedRect -> Shape
drawConnectedRect color fillColor backgroundColor lineWidth cRect =
  [
    Rect.drawRect fillColor cRect.outerRect
    ,Rect.drawRect backgroundColor cRect.innerRect
    ,Rect.drawEmptyRect color lineWidth cRect.outerRect
    ,Rect.drawEmptyRect color lineWidth cRect.innerRect
  ] ++ drawLinesBetweenRects color lineWidth cRect
    |> group

drawLinesBetweenRects : Color -> Number -> ConnectedRect -> List(Shape)
drawLinesBetweenRects color lineWidth cRect =
  List.map (Line.drawLine color lineWidth) 
    (linesBetweenRects cRect)


linesBetweenConnectedPairs : Float -> ConnectedRect -> List(Line)
linesBetweenConnectedPairs percent cRect =
  List.map 
    (\(a,b) -> Line.lineBetweenlines percent a b) 
    (connectedLinesPairs cRect)

connectedLinesPairs : ConnectedRect -> List(Line, Line)
connectedLinesPairs cRect =
  let 
    lines = linesBetweenRects cRect
  in
    [Maybe.withDefault (Line (Position 0 0) (Position 0 0)) (List.head lines)]
      |> List.append (Maybe.withDefault [] (List.tail lines))
      |> List.map2 Tuple.pair lines

linesBetweenRects : ConnectedRect -> List(Line)
linesBetweenRects cRect =
  List.map2 
      (Line.linesBetweenLines cRect.segments) 
      (Rect.edges cRect.innerRect) 
      (Rect.edges cRect.outerRect)
    |> List.concat