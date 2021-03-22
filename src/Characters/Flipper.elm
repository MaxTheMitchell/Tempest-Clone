module Characters.Flipper exposing(..)

import Characters.Character as Character exposing(Character)
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Polygon as Poly
import Shapes.Line as Line

import Playground exposing(Screen, Color, Shape)
type alias Flipper = Character.Character

drawFlipper : Screen -> Int -> ConnectedPolygon -> Flipper -> Shape
drawFlipper screen lineWidth cPoly flipper =
  let
    line = Character.characterLine cPoly flipper
    line2 = Character.characterLine cPoly (Character flipper.x (flipper.y - filpperHeight))  
  in
    [
        line.pos1
        ,line2.pos2
        , line.pos2
        ,line2.pos1
    ]
      |> (Poly.drawPoly screen color lineWidth)

speed : Float
speed = 0.1

color : Color 
color = Playground.red 

filpperHeight : Float
filpperHeight = 0.1