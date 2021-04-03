module Levels.Levels exposing(..)

import Levels.Level as Level exposing(..)

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
  let
      orgin = Position 0.3 0.3
      updateEvents1 : Level -> Level
      updateEvents1 level =  
       case (Level.toRecord level).count of 
        20 -> Level.addEnimes [Flipper (Flipper.initFlipper 0)] level
        60 -> Level.addEnimes 
                [Flipper (Flipper.initFlipper 4)
                ,Flipper (Flipper.initFlipper 8)]
                level
        120 -> Level.addEnimes 
                [Flipper (Flipper.initFlipper 5)
                ,Flipper (Flipper.initFlipper 2)
                ,Flipper (Flipper.initFlipper 10)]
                level
        200 -> List.range 0 9
          |> List.map (\n -> Flipper (Flipper.initFlipper (n*2)))
          |> (\e -> Level.addEnimes e level)
        _ -> level 
      
      drawEvents1 : Level -> Level
      drawEvents1 level =
        let
          i = (Level.toRecord level).count
        in
        if i > 400
            then Level.finish (1.001 * toFloat (i-400)) orgin level
            else if i < 20 then Level.finish (toFloat i/20) orgin level  
            else level
  in
  
  [
    levelInit
      (ConnectedPolygon
          20
          (Poly.equaladeral 5 orgin 0.1)
          (Poly.equaladeral 5 (Position 0 0) 0.9))
      updateEvents1
      drawEvents1
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
    (\level -> level)
    (\level -> level)
