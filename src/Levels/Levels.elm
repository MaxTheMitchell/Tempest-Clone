module Levels.Levels exposing(..)

import Levels.Level as Level exposing(Level, levelInit)

import Shapes.ConnectedPolygon exposing(ConnectedPolygon)
import Shapes.Polygon as Poly exposing(Polygon)
import Shapes.Position exposing(Position)
import Characters.Character exposing(Character)
import Characters.Enimies exposing(..)

import Playground exposing (Screen, Shape)
import Array exposing(Array) 
import Playground exposing (Keyboard)
import Characters.Flipper exposing (Flipper)

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
          (Poly.equaladeral 3 (Position 0 0) 0.1)
          (Poly.equaladeral 5 (Position 0 0) 0.9))
      (\i -> case i of 
        10 -> [Flipper (Character 0 1)]
        60 -> [Flipper (Character 4 1)
              ,Flipper (Character 8 1)
              ]
        120 -> [Flipper (Character 5 1)
              ,Flipper (Character 2 1)
              ,Flipper (Character 10 1)
              ]
        _ -> []
      )
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
    (\_ -> [])

lineWidth : Float
lineWidth = 3
