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
  if (Array.length levels.levels) <= levels.count
    then levelsInit
    else
      Levels
        (
          if Level.isFinshed (currentLevel levels)
            then (levels.count + 1)
            else levels.count  
        )
        (
          levels 
            |> currentLevel
            |> (Level.updateLevel keyboard)
            |> (\current -> Array.set levels.count current levels.levels) 
        )

levelsInit : Levels
levelsInit = 
  let
      orgin1 = Position 0.35 0.35
      orgin2 = Position 1 0
      zeroOrgin = Position 0 0

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
            then Level.finish (1.001 * toFloat (i-400)) orgin1 level
            else if i < 20 then Level.finish (toFloat i/20) orgin1 level  
            else level

      updateEvents2 : Level -> Level
      updateEvents2 level = 
        let 
          enimies i = 
            List.range 0 4
              |> List.map (\n -> Flipper (Flipper.initFlipper (n*2+i)))
              |> (\e -> Level.addEnimes e level)
        in  
        case (Level.toRecord level).count of 
          20 -> enimies 0
          120 -> enimies 10
          220 -> enimies 20
          _ -> level 

      drawEvents2 : Level -> Level
      drawEvents2 level =
        let
          i = (Level.toRecord level).count
        in
        if i > 400
            then Level.finish (1.001 * toFloat (i-400)) zeroOrgin level
            else if i < 20 then Level.finish (toFloat i/20) zeroOrgin level  
            else level

      updateEvents3 : Level -> Level
      updateEvents3 level =   
        let
          count = (Level.toRecord level).count 
        in
          if modBy 150 count == 0 then 
            Level.addEnimes 
                [Flipper (Flipper.initFlipper 0)
                ,Flipper (Flipper.initFlipper 2)
                ,Flipper (Flipper.initFlipper 4)
                ,Flipper (Flipper.initFlipper 6)
                ,Flipper (Flipper.initFlipper 8)
                ]
                level
          else if modBy 75 count == 0 then
            Level.addEnimes 
                [Flipper (Flipper.initFlipper 1)
                ,Flipper (Flipper.initFlipper 3)
                ,Flipper (Flipper.initFlipper 5)
                ,Flipper (Flipper.initFlipper 7)
                ,Flipper (Flipper.initFlipper 9)
                ]
                level
          else level

      updateEvents4 : Level -> Level
      updateEvents4 level =
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

      drawEvents3 : Level -> Level
      drawEvents3 level =
        let
          i = (Level.toRecord level).count
        in
        if i > 400
            then Level.finish (1.001 * toFloat (i-400)) orgin2 level
            else if i < 20 then Level.finish (toFloat i/20) orgin2 level  
            else level
  in
  
  [
    levelInit
      420
      (ConnectedPolygon
          20
          (Poly.equaladeral 5 orgin1 0.1)
          (Poly.equaladeral 5 zeroOrgin 0.9))
      updateEvents1
      drawEvents1
    , 
    levelInit
      420
      (ConnectedPolygon
        30
        (Poly.equaladeral 3 zeroOrgin 0.1)
        (Poly.equaladeral 3 zeroOrgin 0.9))
      updateEvents2
      drawEvents2
    ,levelInit
      420
      (ConnectedPolygon
        10
        (Poly.starShape 10 zeroOrgin 0.1)
        (Poly.starShape 10 zeroOrgin 0.9))
      updateEvents3
      drawEvents2
    ,levelInit
      420
      (ConnectedPolygon
        24
        (Poly.equaladeral 4 zeroOrgin 0.9)
        (Poly.equaladeral 4 zeroOrgin 0.1))
      updateEvents2
      drawEvents2
    ,
    levelInit
      420
      (ConnectedPolygon
        25
        (Poly.equaladeral 5 orgin2 0.1)
        (Poly.equaladeral 5 zeroOrgin 0.9))
      updateEvents4
      drawEvents3
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
    0
    (ConnectedPolygon
      0
      (Poly.equaladeral 0 (Position 0 0) 0)
      (Poly.equaladeral 0 (Position 0 0) 0)     
    )
    (\level -> level)
    (\level -> level)
