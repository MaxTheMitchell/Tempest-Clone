module Main exposing(main, segments)

import Playground exposing (..)
import Shapes.Position exposing(Position)
import Shapes.Line as Line exposing(..)
import Characters.Bullet as Bullet
import Characters.Player as Player exposing(Player)
import Shapes.Polygon as Poly
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Characters.Character exposing(Character)


type alias Memory =
    { player : Player
    , bullets : List(Bullet.Bullet)
    , cPoly : ConnectedPolygon
    , count : Int 
    }

main =
  let 
    outerRect = Poly.square (Position 0 0) 0.9
    innerRect = Poly.square (Position 0 0) 0.1
    -- outerRect = [
    --    (Position 0.6 -0)
    --    ,(Position 0 0.9)
    --   , (Position -0.6 0)
    --   ,(Position 0 -0.9)
    --   ]
    -- innerRect = [
    --    (Position 0.1 -0)
    --    ,(Position 0 0.15)
    --   , (Position -0.1 0)
    --   ,(Position 0 -0.15)
    --   ]
    rect = ConnectedPolygon segments innerRect outerRect
  in
    game view update (Memory (Character 0 0) []  rect 0)


view computer memory =
    [
      fillScreen backgroundColor computer.screen
      ,CPoly.drawConnectedPoly computer.screen shapeColor lineWidthConst memory.cPoly
      ,Player.drawPlayer computer.screen yellow lineWidthConst memory.cPoly memory.player
      ,Bullet.drawBullets computer.screen green lineWidthConst memory.cPoly memory.bullets
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
    (Bullet.updateBullets computer.keyboard memory.player memory.bullets)
    memory.cPoly
    (memory.count + 1)

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