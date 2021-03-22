module Characters.Player exposing(..)

import Playground exposing(Shape, Color, Keyboard)
import Shapes.Line as Line exposing(Line)
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Position as Position exposing(Position)
import Characters.Character as Character exposing(Character)
import Array

type alias Player = Character

drawPlayer : Playground.Screen -> Color -> Int -> ConnectedPolygon -> Player -> Shape
drawPlayer screen color size cPoly player =
  let 
    line = Character.characterLine cPoly player
    slope = Line.slope line
    center = Position 
      ((Line.lineCenter line).x +  (slope.y * playerSize * -1)) --this negitve one fixes the "arrow" pointing for the player. Need to look into more at some point
      ((Line.lineCenter line).y +  (slope.x * playerSize))
  in
    Playground.group 
      [
        Line.drawLine screen color size (Line line.pos1 center)
        ,Line.drawLine screen  color size (Line line.pos2 center)
      ]

move : Playground.Keyboard -> ConnectedPolygon -> Player -> Player
move keyboard cPoly player =
  player
    |> (moveX (truncate (Playground.toX keyboard)) cPoly)
    |> (moveY (truncate (Playground.toY keyboard)))

moveX : Int -> ConnectedPolygon -> Player -> Player
moveX dir cPoly player = 
  Character 
  (
    dir
      |> (+) player.x
      |> (+) cPoly.segments 
      |> modBy cPoly.segments
  )
  player.y

moveY : Int -> Player -> Player
moveY dir player = 
  Character
    player.x
    (
      dir
        |> toFloat 
        |> (*) speed
        |> (+) player.y
        |> (bound 0 1)
    )

playerSize : Float
playerSize = 0.2

speed : Float
speed = 0.1

bound : Float -> Float -> Float -> Float
bound min max val =
  if min > val then
    min
  else if max < val then 
    max
  else
    val