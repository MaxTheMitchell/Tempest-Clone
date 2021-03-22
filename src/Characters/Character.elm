module Characters.Character exposing(..)

import Shapes.ConnectedPolygon as CPoly exposing(ConnectedPolygon)
import Shapes.Line as Line exposing(Line)
import Shapes.Position as Position exposing(Position)
import Array

type alias Character =
    {x : Int
    , y : Float
    }

characterLine : ConnectedPolygon -> Character -> Line
characterLine cPoly character =
  cPoly
    |> (CPoly.linesBetweenConnectedPairs character.y)
    |> Array.fromList
    |> Array.get character.x
    |> Maybe.withDefault (Line (Position 0 0) (Position 0 0)) 