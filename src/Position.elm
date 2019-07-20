module Position exposing (EndPosition, Position, StartPosition, angle, calcDistance, decoder, shift, snap, toString)

import Json.Decode as D


type alias Position =
    { x : Int
    , y : Int
    }


type alias StartPosition =
    Position


type alias EndPosition =
    Position


decoder : D.Decoder Position
decoder =
    D.map2 Position
        (D.field "x" D.int)
        (D.field "y" D.int)


toString : Position -> String
toString { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


shift : number -> number -> { x : number, y : number } -> { x : number, y : number }
shift dx dy pos =
    { pos | x = pos.x + dx, y = pos.y + dy }


calcDistance : Position -> Position -> Float
calcDistance a b =
    sqrt <| toFloat <| (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2


angle : Position -> Position -> Float
angle a b =
    let
        theta =
            atan2 (toFloat (b.y - a.y)) (toFloat (b.x - a.x))

        radians =
            if theta < 0 then
                (2 * pi) + theta

            else
                theta
    in
    radians


{-| Returns the position of the nearest 45 degree angle
-}
snap : StartPosition -> EndPosition -> EndPosition
snap start curPos =
    angle start curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance start curPos)
        |> shift start.x start.y


toDeltas : Float -> Float -> Position
toDeltas h theta =
    Position (round (cos theta * h)) (round (sin theta * h))
