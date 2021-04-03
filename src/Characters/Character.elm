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
    line = characterLine cPoly character
  in  
    Poly.starShape 
      25 
      (Line.lineCenter line) 
      ((Line.lineSize line)/2)
      |> Poly.drawPoly screen character.color lineWidth

nullCharacter : Character 
nullCharacter = Character 0 0 0 Playground.black