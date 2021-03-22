module Levels.Level exposing(..)

import Playground exposing(Shape, Screen, Color, Keyboard, green, yellow, red, rgb)

import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Characters.Character exposing(Character)
import Characters.Player as Player exposing(Player)
import Characters.Bullet as Bullet exposing(Bullet)
import Characters.Flipper as Flipper
import Characters.Enimies as Enimies exposing(Enimie)

type alias Level = 
  {
  count : Int 
  , cPoly : ConnectedPolygon
  , player : Player
  , bullets : List(Bullet)
  , enimies : List(Enimie)
  , events : Int -> List(Enimie)
  }

drawLevel : Screen -> Level -> Shape
drawLevel screen level =
  [
    CPoly.drawConnectedPoly screen shapeColor lineWidth level.cPoly
    ,Player.drawPlayer screen yellow lineWidth level.cPoly level.player
    ,Bullet.drawBullets screen green lineWidth level.cPoly level.bullets
    ,Enimies.drawEnimies screen lineWidth level.cPoly level.enimies
  ] |> Playground.group

updateLevel : Keyboard -> Level -> Level
updateLevel keyboard level =
  Level
    (level.count + 1)
    level.cPoly
    (
      if modBy updateCount level.count == 0 then 
        Player.move keyboard level.cPoly level.player
      else
        level.player
    )
    (Bullet.updateBullets keyboard level.player level.bullets)
    (level.enimies ++ level.events level.count)
    level.events
    

levelInit : ConnectedPolygon -> (Int -> List(Enimie)) -> Level
levelInit cPoly events =
  Level
    0
    cPoly
    (Character 0 0)
    []
    []
    events

lineWidth : Int
lineWidth = 3

shapeColor : Color
shapeColor = rgb 0 0 255

updateCount : Int
updateCount = 3
