module Characters.Bullet exposing(..)

import Playground exposing(Color, Shape)

import Characters.Character as Character exposing(Character)
import Shapes.Polygon as Poly exposing(Polygon)
import Shapes.Position as Position exposing(Position)
import Shapes.Line as Line
import Shapes.ConnectedPolygon exposing(ConnectedPolygon)
type alias Bullet = Character

drawBullets : Playground.Screen -> Color -> Int -> ConnectedPolygon -> List(Bullet) -> Shape
drawBullets screen color size cPoly bullets = 
    bullets
      |> (List.map (drawBullet screen color size cPoly))
      |> Playground.group 

drawBullet : Playground.Screen -> Color -> Int -> ConnectedPolygon -> Bullet -> Shape
drawBullet screen color size cPoly bullet = 
  let
    line = Character.characterLine cPoly bullet
  in
    Poly.equaladeral 3 (Line.lineCenter line) ((Line.lineSize line)*bulletSize/2) 
      |> (Poly.drawPoly screen color size)

updateBullets : Playground.Keyboard -> Character -> List(Bullet) -> List(Bullet)
updateBullets keyboard player bullets =
    (if isShooting keyboard then
        addBullet player bullets
    else
      bullets)
      |> List.map move
      |> List.filter isInBounds

addBullet : Character -> List(Bullet) -> List(Bullet)
addBullet player bullets =
  (Character player.x player.y) :: bullets 

move : Bullet -> Bullet
move bullet = 
  Character
    bullet.x
    (bullet.y + bulletSpeed)

isInBounds : Bullet -> Bool
isInBounds bullet =
  bullet.y <= 1 && bullet.y >= 0

isShooting : Playground.Keyboard -> Bool
isShooting keyboard = keyboard.space == True

bulletSize : Float
bulletSize = 0.1

bulletSpeed : Float 
bulletSpeed = 0.05