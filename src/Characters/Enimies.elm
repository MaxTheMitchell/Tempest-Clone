module Characters.Enimies exposing(..)

import Characters.Flipper as Flipper exposing(Flipper)
import Characters.Character exposing(Character)
import Shapes.ConnectedPolygon exposing(ConnectedPolygon)

import Playground exposing(Shape, Screen, Color)


type Enimie
  = Flipper Character

drawEnimies : Screen -> Int -> ConnectedPolygon -> List(Enimie) -> Shape
drawEnimies screen lineWidth cPoly enimies =
  enimies
    |> List.map (\e -> 
      case e of 
      Flipper c -> Flipper.drawFlipper screen lineWidth cPoly c
    )
    |> Playground.group