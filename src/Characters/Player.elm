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
  = Alive Character Int
  | Dying Character Int
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
    Alive _ _ -> 
      [
        line.pos1
        ,center
        , line.pos2
        , Position (center.x + (slope.y * playerSize * -1)) (center.y + (slope.x * playerSize))
      ] |> (Poly.drawPoly screen color size)
    Dying c _ -> Character.drawDead screen size cPoly c
    Dead -> Playground.rectangle black 0 0
    
updatePlayer : Playground.Keyboard -> ConnectedPolygon -> List(Enimie) -> Player -> Player
updatePlayer keyboard cPoly enimies player =
  let 
    moveInterval = 2
    deathInterval = 5
  in
    case player of 
    Alive c i -> 
      if playerHit enimies c then
        Dying c 0
      else 
        if i > moveInterval then 
          c 
            |> (moveX (truncate (Playground.toX keyboard)) cPoly)
            |> (\ch -> Alive ch 0)
        else 
          Alive c (if (Playground.toX keyboard)== 0 then moveInterval else i+1 )
    Dying c i ->
      if i > deathInterval then 
        Dead
      else 
        Dying c (i+1)
    Dead -> Dead
      

initPlayer : Player
initPlayer = Alive (Character 0 0 playerSize color) 0

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
  player.color

moveY : Int -> Float -> Player -> Player
moveY dir speed playerType = 
 let
     player = toCharacter playerType
 in
  Alive 
    (Character
      player.x
      (
        dir
          |> toFloat 
          |> (*) speed
          |> (+) player.y
          |> (bound 0 1)
      )
      player.height
      player.color) 0

isDead : Player -> Bool
isDead player =
  case player of 
  Dead -> True
  _ -> False

toCharacter : Player -> Character
toCharacter player =
  case player of 
  Alive c _ -> c
  Dying c _ -> c
  Dead -> Character.nullCharacter


playerSize : Float
playerSize = 0.2

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