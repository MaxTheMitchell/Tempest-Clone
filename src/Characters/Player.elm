module Characters.Player exposing(..)

import Playground exposing(Shape, Color, Keyboard)
import Shapes.Line as Line exposing(Line)
import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Polygon as Poly exposing(Polygon)
import Shapes.Position as Position exposing(Position)
import Characters.Character as Character exposing(Character)
import Characters.Enimies as Enimies exposing(Enimie)
import Array
import Characters.Flipper exposing (Flipper(..))
import Html.Attributes exposing (height)
import Playground exposing (black)

type Player
  = Alive Character
  | Dying Character
  | Dead

drawPlayer : Playground.Screen -> Int -> ConnectedPolygon -> Player -> Shape
drawPlayer screen size cPoly player =
  let 
    line = Character.characterLine cPoly (toCharacter player)
    height = (toCharacter player).height
    slope = Line.slope line
    center = Position 
      ((Line.lineCenter line).x + (slope.y * height * -1)) --this negitve one fixes the "arrow" pointing for the player. Need to look into more at some point
      ((Line.lineCenter line).y + (slope.x * height))
  in
    case player of 
    Alive _ -> 
      [
        line.pos1
        ,center
        , line.pos2
        , Position (center.x + (slope.y * playerSize * -1)) (center.y + (slope.x * playerSize))
      ] |> (Poly.drawPoly screen color size)
    Dying c -> Character.drawDead screen size cPoly c
    Dead -> Playground.rectangle black 0 0
    
updatePlayer : Playground.Keyboard -> ConnectedPolygon -> List(Enimie) -> Player -> Player
updatePlayer keyboard cPoly enimies player =
  case player of 
  Alive c -> 
    if playerHit enimies c then
      Dying c
    else 
      if Character.shouldUpdate c then 
        c 
          |>(moveX (truncate (Playground.toX keyboard)) cPoly)
          |> Character.updateCharacter
          |> Alive
      else 
        c
          |> Character.updateCharacter
          |> Alive
  Dying c ->
    if Character.shouldUpdate c then 
      Dead
    else 
      c
        |> Character.updateCharacter
        |> Dying
  Dead -> Dead
      

initPlayer : Player
initPlayer = Alive (Character 0 0 playerSize 0 updateInterval color)

playerHit : List(Enimie) -> Character -> Bool
playerHit enimies player =
  enimies 
    |> Enimies.toCharacters
    |> List.any (\e -> Character.onRim e && e.x == player.x)

moveX : Int -> ConnectedPolygon -> Character -> Character
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
  player.updateCount
  player.updateInterval
  player.color

moveY : Int -> Character -> Character
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
    player.updateCount
    player.updateInterval
    player.color

isDead : Player -> Bool
isDead player =
  case player of 
  Dead -> True
  _ -> False

toCharacter : Player -> Character
toCharacter player =
  case player of 
  Alive c -> c
  Dying c -> c
  Dead -> Character.nullCharacter


updateInterval : Int 
updateInterval = 3
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