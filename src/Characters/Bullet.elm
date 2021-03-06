module Characters.Bullet exposing(..)

import Playground exposing(Color, Shape)

import Characters.Character as Character exposing(Character)
import Shapes.Polygon as Poly exposing(Polygon)
import Shapes.Position as Position exposing(Position)
import Shapes.Line as Line
import Shapes.ConnectedPolygon exposing(ConnectedPolygon)
import Characters.Enimies as Enimies exposing(Enimie)

type alias Bullet = Character

drawBullets : Playground.Screen -> Int -> ConnectedPolygon -> List(Bullet) -> Shape
drawBullets screen lineWidth cPoly bullets = 
    bullets
      |> List.map (drawBullet screen lineWidth cPoly)
      |> Playground.group 

drawBullet : Playground.Screen -> Int -> ConnectedPolygon -> Bullet -> Shape
drawBullet screen size cPoly bullet = 
  let
    line = Character.characterLine cPoly bullet
  in
    Poly.equaladeral 3 (Line.lineCenter line) ((Line.lineSize line)*bullet.height/2) 
      |> (Poly.drawPoly screen bullet.color size)

updateBullets : Playground.Keyboard -> Int -> Character -> List(Enimie) -> List(Bullet) -> List(Bullet)
updateBullets keyboard levelCount player enimies bullets =
    (if isShooting keyboard && modBy 5 levelCount == 0 then
        addBullet player bullets
    else
      bullets)
      |> List.filter (\b -> 
        (isInBounds b) 
        && not (List.any (Character.charactersIntersecting b) (Enimies.toCharacters enimies))
        ) 
      |> List.map move

addBullet : Character -> List(Bullet) -> List(Bullet)
addBullet player bullets =
  (Character player.x player.y bulletSize bulletColor) :: bullets 

move : Bullet -> Bullet
move bullet = 
  Character
    bullet.x
    (bullet.y + bulletSpeed)
    bullet.height
    bullet.color

isInBounds : Bullet -> Bool
isInBounds bullet =
  bullet.y <= 1 && bullet.y >= 0

isShooting : Playground.Keyboard -> Bool
isShooting keyboard = keyboard.space == True

bulletColor : Color
bulletColor = Playground.green

bulletSize : Float
bulletSize = 0.1

bulletSpeed : Float 
bulletSpeed = 0.05