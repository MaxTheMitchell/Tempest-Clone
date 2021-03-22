module Characters.Player exposing(..)

import Playground exposing(Shape, Color, Keyboard)
import Shapes.Line as Line exposing(Line)
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Polygon as Poly exposing(Polygon)
import Shapes.Position as Position exposing(Position)
import Characters.Character as Character exposing(Character)
import Array

type alias Player = Character

drawPlayer : Playground.Screen -> Int -> ConnectedPolygon -> Player -> Shape
drawPlayer screen size cPoly player =
  let 
    line = Character.characterLine cPoly player
    slope = Line.slope line
    center = Position 
      ((Line.lineCenter line).x + (slope.y * player.height * -1)) --this negitve one fixes the "arrow" pointing for the player. Need to look into more at some point
      ((Line.lineCenter line).y + (slope.x * player.height))
  in
      [
        line.pos1
        ,center
        , line.pos2
        , Position (center.x + (slope.y * playerSize * -1)) (center.y + (slope.x * playerSize))
      ] |> (Poly.drawPoly screen color size)

move : Playground.Keyboard -> ConnectedPolygon -> Player -> Player
move keyboard cPoly player =
  player
    |> (moveX (truncate (Playground.toX keyboard)) cPoly)
    |> (moveY (truncate (Playground.toY keyboard)))

initPlayer : Player
initPlayer = Character 0 0 playerSize color

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
  player.height
  player.color

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
    player.height
    player.color

playerSize : Float
playerSize = 0.2

speed : Float
speed = 0.1

color : Color
color = Playground.yellow

bound : Float -> Float -> Float -> Float
bound min max val =
  if min > val then
    min
  else if max < val then 
    max
  else
    val