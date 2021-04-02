module Characters.Character exposing(..)

import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)

import Array
import Playground exposing(Color)
import Shapes.Polygon as Poly exposing (Polygon)
import Playground exposing (Shape)

type alias Character =
    {x : Int
    , y : Float
    , height : Float
    , color : Color
    }

characterLine : ConnectedPolygon -> Character -> Line
characterLine cPoly character =
  cPoly
    |> (CPoly.linesBetweenConnectedPairs character.y)
    |> Array.fromList
    |> Array.get character.x
    |> Maybe.withDefault (Line (Position 0 0) (Position 0 0)) 

charactersIntersecting : Character -> Character -> Bool
charactersIntersecting character1 character2 =
  (character1.x == character2.x) && (charactersYInterecting character1 character2)

charactersYInterecting : Character -> Character -> Bool
charactersYInterecting character1 character2 =
  (character1.y >= character2.y && character1.y <= character2.y + character2.height)
  || 
  (character1.y + character1.height >= character2.y && character1.y  + character1.height <= character2.y + character2.height)

onRim : Character -> Bool
onRim character = character.y == 0

drawDead :  Playground.Screen -> Int -> ConnectedPolygon -> Character -> Shape
drawDead screen lineWidth cPoly character =
  let
    points = 25
    center = Line.lineCenter (characterLine cPoly character)
    rotatePoint = (characterLine cPoly character).pos1
    smallerRotatePoint = Line.lineCenter (Line center rotatePoint)
  in
  List.range 0 points
    |> List.map (\i ->
      Position.rotateAroundPoint ((360/points)*toFloat i) center 
      (if modBy 2 i == 0 then rotatePoint else smallerRotatePoint)  
      )
    |> Poly.drawPoly screen character.color lineWidth



nullCharacter : Character 
nullCharacter = Character 0 0 0 Playground.black