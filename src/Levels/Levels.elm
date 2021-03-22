module Levels.Levels exposing(..)

import Levels.Level as Level exposing(Level, levelInit)

import Shapes.ConnectedPolygon exposing(ConnectedPolygon)
import Shapes.Polygon as Poly exposing(Polygon)
import Shapes.Position exposing(Position)

import Characters.Character exposing(Character)

import Playground exposing (Screen, Shape)
import Array exposing(Array) 
import Playground exposing (Keyboard)

type alias Levels = 
  { count : Int
  , levels : Array(Level)
  }

drawLevels : Screen -> Levels -> Shape 
drawLevels screen levels =
  levels
    |> currentLevel
    |> (Level.drawLevel screen)

updateLevels : Keyboard -> Levels -> Levels
updateLevels keyboard levels =
  levels
    |> currentLevel
    |> (Level.updateLevel keyboard)
    |> (\current -> Array.set levels.count current levels.levels) 
    |> (Levels levels.count) 

levelsInit : Levels
levelsInit = 
  [
    levelInit
      (ConnectedPolygon
          20
          (Poly.equaladeral 4 (Position 0 0) 0.1)
          (Poly.equaladeral 4 (Position 0 0) 0.9))
  ] 
    |> Array.fromList
    |> (Levels 0)

currentLevel : Levels -> Level
currentLevel levels = 
  case Array.get levels.count levels.levels of
    Maybe.Nothing -> fakeLevel
    Just level -> level 

fakeLevel : Level
fakeLevel = 
  levelInit
    (ConnectedPolygon
      0
      (Poly.equaladeral 0 (Position 0 0) 0)
      (Poly.equaladeral 0 (Position 0 0) 0)     
    )

lineWidth : Float
lineWidth = 3