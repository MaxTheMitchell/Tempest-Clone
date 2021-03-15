module Shapes.Rect exposing (..)

import Playground exposing(..)
import Shapes.Position exposing(Position)
import Shapes.Line exposing(..)
import Array

type alias Rect =
    { 
      pos : Position
      ,width : Int
      ,height : Int 
    }

square : Position -> Int -> Rect
square pos size =
  Rect pos size size

drawRect : Color -> Rect -> Shape 
drawRect color rect =
  rectangle color (toFloat rect.width) (toFloat rect.height)
    |> move (toFloat rect.pos.x) (toFloat rect.pos.y)  

drawEmptyRect : Color -> Number -> Rect -> Shape
drawEmptyRect color lineWidth rect =
  rect 
    |> edges 
    |> List.map (drawLine color lineWidth)
    |> group

edges : Rect -> List(Line)
edges rect =
  let
    rectCorner i = getCorner i rect
  in
  [
    Line (rectCorner 1) (rectCorner 2)
    , Line (rectCorner 0) (rectCorner 1)
    , Line (rectCorner 3) (rectCorner 0)
    , Line (rectCorner 2) (rectCorner 3) 
  ]

getCorner : Int -> Rect -> Position
getCorner i rect =
  Array.fromList(corners rect)
    |> Array.get i
    |> Maybe.withDefault (Position 0 0)

corners : Rect -> List(Position)
corners rect =
  let 
    halfWidth = rect.width//2
    halfHeight = rect.height//2
  in
    [
    Position (rect.pos.x - halfWidth) (rect.pos.y - halfHeight)
    ,Position (rect.pos.x + halfWidth) (rect.pos.y - halfHeight)
    ,Position (rect.pos.x + halfWidth) (rect.pos.y + halfHeight)
    ,Position (rect.pos.x - halfWidth) (rect.pos.y + halfHeight)
    ]
