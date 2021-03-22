module Characters.Enimies exposing(..)

import Characters.Flipper as Flipper exposing(Flipper)
import Characters.Character as Character exposing(Character)
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

updateEnimies : List(Character) -> List(Enimie) -> List(Enimie)
updateEnimies bullets enimies =
  enimies
    |> List.map (\e ->
      case e of
      Flipper c -> Flipper (Flipper.updateFlipper c)
    )
    |> List.filter 
      (\e -> not (List.any 
          (Character.charactersIntersecting (toCharacter e))
          bullets
          ))

toCharacters : List(Enimie) -> List(Character)
toCharacters = List.map toCharacter

toCharacter : Enimie -> Character
toCharacter enimie =
  case enimie of 
  Flipper c -> c