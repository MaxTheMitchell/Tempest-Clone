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
    }

main =
  game view update (Memory (Player 0 0) 0)


view computer memory =
    testVeiw (Position 0 0) computer.screen memory

update : Computer -> Memory -> Memory
update computer memory =
  Memory
  (
    if modBy updateCount memory.count == 0 then 
      Player
        (
          toX computer.keyboard
          |> floor
          |> (+) memory.player.x
          |> (+) (4 * segments) 
          |> modBy (4 * segments)
        )
        (
          toY computer.keyboard
          |> (*) yMove
          |> (+) memory.player.y
        )
    else
      memory.player
  )
     (memory.count + 1)


testVeiw : Position -> Screen -> Memory -> List(Shape)
testVeiw pos screen memory =
  let 
    outerSize = (floor (min screen.height screen.width) - 100)
    outerRect = Poly.square pos outerSize
    innerRect = Poly.square (Position 0 0) (floor (toFloat (outerSize) * 0.1))
    rect = ConnectedPolygon segments innerRect outerRect
  in
    [
      fillScreen backgroundColor screen
      ,CPoly.drawConnectedPoly shapeColor lineWidthConst rect
      ,Player.drawPlayer yellow lineWidthConst rect memory.player
      ,CPoly.linesBetweenConnectedPairs 0.5 rect 
        |> List.map (Shapes.Line.drawLine green 3 )
        |> group
    ]

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
segments = 2

updateCount : Int
updateCount = 3

yMove : Float
yMove = 0.1