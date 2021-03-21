module Main exposing(main, segments)

import Playground exposing (..)
import Shapes.Position exposing(Position)
import Shapes.Line as Line exposing(..)
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
    outerRect = Poly.square (Position 0 0) 0.9
    innerRect = Poly.square (Position 0 0) 0.2
    -- outerRect = [
    --   (Position -400 -400)
    --   , (Position 400 -400)
    --   , (Position 0 400)]
    
    -- innerRect = [
    --    (Position 0.1 -0.1)
    --    ,(Position 0 0.2)
    --   , (Position -0.1 -0.1)
    --   ]
    rect = ConnectedPolygon segments innerRect outerRect
  in
    game view update (Memory (Player 0 0) 0 rect)


view computer memory =
    [
      fillScreen backgroundColor computer.screen
      ,CPoly.drawConnectedPoly computer.screen shapeColor lineWidthConst memory.cPoly
      ,Player.drawPlayer computer.screen yellow lineWidthConst memory.cPoly memory.player
      -- , Line.drawLine computer.screen red 5 (Line (Position 0.5 0) (Position 0 0))
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