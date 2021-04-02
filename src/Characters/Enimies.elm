module Characters.Enimies exposing(..)

import Characters.Flipper as Flipper exposing(Flipper)
import Characters.Character as Character exposing(Character)
import Shapes.ConnectedPolygon exposing(ConnectedPolygon)

import Playground exposing(Shape, Screen, Color)

type Enimie
  = Flipper Flipper

drawEnimies : Screen -> Int -> ConnectedPolygon -> List(Enimie) -> Shape
drawEnimies screen lineWidth cPoly enimies =
  enimies
    |> List.map (\e -> 
      case e of 
      Flipper f -> Flipper.drawFlipper screen lineWidth cPoly f
    )
    |> Playground.group

updateEnimies : List(Character) -> List(Enimie) -> List(Enimie)
updateEnimies bullets enimies =
  enimies
    |> List.map (\e ->
      case e of
      Flipper c -> Flipper (Flipper.updateFlipper bullets c)
    )
    |> List.filter (\e -> not (dead e)) 

toCharacters : List(Enimie) -> List(Character)
toCharacters = List.map toCharacter

toCharacter : Enimie -> Character
toCharacter enimie =
  case enimie of 
  Flipper c -> Flipper.toCharacter c

dead : Enimie -> Bool
dead enimie = 
  case enimie of 
  Flipper f -> Flipper.dead f