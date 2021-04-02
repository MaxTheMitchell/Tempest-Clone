module Characters.Flipper exposing(..)

import Characters.Character as Character exposing(Character)
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Polygon as Poly
import Shapes.Line as Line

import Playground exposing(Screen, Color, Shape)

type Flipper
  = MoveX Character
  | MoveY Character
  | Dying Character
  | Dead

drawFlipper : Screen -> Int -> ConnectedPolygon -> Flipper -> Shape
drawFlipper screen lineWidth cPoly flipper =
  let
    flipperChacter = toCharacter flipper
    line = Character.characterLine cPoly flipperChacter
    line2 = Character.characterLine 
      cPoly 
      (Character 
        flipperChacter.x 
        (flipperChacter.y - flipperChacter.height) 
        flipperChacter.height
        flipperChacter.updateCount
        flipperChacter.updateInterval
        flipperChacter.color)  
    poly = [line.pos1 ,line2.pos2 ,line.pos2,line2.pos1]
  in
    case flipper of 
      Dying c -> Character.drawDead screen lineWidth cPoly c
      _ -> Poly.drawPoly screen flipperChacter.color lineWidth poly

updateFlipper : List(Character) -> Flipper -> Flipper
updateFlipper bullets flipper =
  let
    checkedFlipper = if List.any (Character.charactersIntersecting (toCharacter flipper)) bullets
      then kill flipper
      else flipper
  in
    case checkedFlipper of 
    MoveY c ->
      MoveY
      (Character
        c.x
        (bound (c.y - speed)) 
        c.height
        c.updateCount
        c.updateInterval
        c.color)
    Dying c -> if Character.shouldUpdate c
      then Dead
      else Dying (Character.updateCharacter c)
    _ -> Dead

initFlipper : Int -> Flipper
initFlipper x =
  MoveY (Character x 1 filpperHeight 1 5 color)

kill : Flipper -> Flipper 
kill flipper =
  case flipper of 
  MoveX c -> Dying c
  MoveY c -> Dying c
  Dying _ -> flipper
  Dead -> Dead

bound : Float -> Float
bound y =
  if y < 0 then
    0
  else 
    y


dead : Flipper -> Bool
dead flipper =
  case flipper of
  MoveX _ -> False
  MoveY _ -> False
  Dying _ -> False
  Dead -> True 

toCharacter : Flipper -> Character
toCharacter flipper =
  case flipper of 
  MoveX c -> c
  MoveY c -> c
  Dying c -> c
  Dead -> Character.nullCharacter 

speed : Float
speed = 0.005

color : Color 
color = Playground.red 

filpperHeight : Float
filpperHeight = 0.05