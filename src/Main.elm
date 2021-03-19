module Main exposing(main, segments)

import Playground exposing (..)
import Shapes.Position exposing(Position)
import Shapes.Line exposing(..)
import Characters.Player as Player exposing(Player)
import Shapes.Polygon as Poly
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)

type alias Memory =
    { player : Player
    , count : Int 
    , cPoly : ConnectedPolygon
    }

main =
  let 
    outerSize = 600
    outerRect = Poly.rect (Position 0 0) outerSize (outerSize)
    -- innerRect = Poly.square (Position 0 0) (floor (toFloat (outerSize) * 0.1))
    -- outerRect = [
    --   (Position -400 -400)
    --   , (Position 400 -400)
    --   , (Position 0 400)]
    
    innerRect = [
       (Position -200 -200)
       ,(Position 0 (200 - 346))
      , (Position 200 -200)
      , (Position 0 (346 - 200))
      
      ]
    rect = ConnectedPolygon segments innerRect outerRect
  in
    game view update (Memory (Player 0 0) 0 rect)


view computer memory =
    [
      fillScreen backgroundColor computer.screen
      ,CPoly.drawConnectedPoly shapeColor lineWidthConst memory.cPoly
      ,Player.drawPlayer yellow lineWidthConst memory.cPoly memory.player
    ]

update : Computer -> Memory -> Memory
update computer memory =
  Memory
    (
      if modBy updateCount memory.count == 0 then 
        Player.move computer.keyboard yMove memory.cPoly memory.player
      else
        memory.player
    )
    (memory.count + 1)
    memory.cPoly

fillScreen : Color -> Screen -> Shape
fillScreen color screen =
  rectangle color screen.width screen.height

lineWidthConst : Int
lineWidthConst = 3

shapeColor : Color
shapeColor = red
backgroundColor : Color
backgroundColor = black

segments : Int
segments = 4*3

updateCount : Int
updateCount = 3

yMove : Float
yMove = 0.1