module Levels.Level exposing(..)

import Playground exposing(Shape, Screen, Color, Keyboard, green, yellow, red, rgb)

import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Characters.Character exposing(Character)
import Characters.Player as Player exposing(Player)
import Characters.Bullet as Bullet exposing(Bullet)
import Characters.Flipper as Flipper
import Characters.Enimies as Enimies exposing(Enimie)
import Playground exposing (black)

type alias LevelRecord = 
  {
    count : Int 
    , cPoly : ConnectedPolygon
    , player : Player
    , bullets : List(Bullet)
    , enimies : List(Enimie)
    , events : (Int -> List(Enimie))
    }
type Level = 
  Level LevelRecord

drawLevel : Screen -> Level -> Shape
drawLevel screen (Level level) =
  [
    CPoly.drawConnectedPoly screen shapeColor lineWidth level.cPoly
    ,Player.drawPlayer screen lineWidth level.cPoly level.player
    ,Bullet.drawBullets screen lineWidth level.cPoly level.bullets
    ,Enimies.drawEnimies screen lineWidth level.cPoly level.enimies
  ] |> Playground.group

updateLevel : Keyboard -> Level -> Level
updateLevel keyboard (Level level) =
  if Player.isDead level.player then 
    reset (Level level) 
  else 
    Level
      {
      count = level.count + 1
      , cPoly = level.cPoly
      , player = (Player.updatePlayer keyboard level.cPoly level.enimies level.player)
      , bullets = (Bullet.updateBullets keyboard (Player.toCharacter level.player) level.enimies level.bullets)
      , enimies = ((Enimies.updateEnimies level.bullets level.enimies) ++ level.events level.count)
      , events = level.events
      }
    

levelInit : ConnectedPolygon -> (Int -> List(Enimie)) -> Level
levelInit cPoly events =
  Level
    {
    count = 0
    ,cPoly = cPoly
    ,player = Player.initPlayer
    ,bullets = []
    ,enimies = []
    ,events = events
    }

reset : Level -> Level
reset (Level level) = 
  levelInit level.cPoly level.events

toRecord : Level -> LevelRecord
toRecord (Level record) = record
    
lineWidth : Int
lineWidth = 1

shapeColor : Color
shapeColor = rgb 0 0 255

updateCount : Int
updateCount = 3
