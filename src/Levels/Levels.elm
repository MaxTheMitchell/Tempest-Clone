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
import Characters.Flipper as Flipper exposing (Flipper)

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
          (Poly.equaladeral 5 (Position 0.3 0.3) 0.1)
          (Poly.equaladeral 5 (Position 0 0) 0.9))
      (\i -> case i of 
        10 -> [Flipper (Flipper.initFlipper 0)]
        60 -> [Flipper (Flipper.initFlipper 4)
              ,Flipper (Flipper.initFlipper 8)
              ]
        120 -> [Flipper (Flipper.initFlipper 5)
              ,Flipper (Flipper.initFlipper 2)
              ,Flipper (Flipper.initFlipper 10)
              ]
        200 -> List.range 0 10
          |> List.map (\n -> Flipper (Flipper.initFlipper (n*2)))
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
