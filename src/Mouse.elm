module Mouse exposing (Position, position)

import Json.Decode as D
import Json.Encode as E


type alias Position =
    { x : Int, y : Int }


position : D.Decoder Position
position =
    D.map2 Position
        (D.field "x" D.int)
        (D.field "y" D.int)
