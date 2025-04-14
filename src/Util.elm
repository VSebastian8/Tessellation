module Util exposing (..)

import ColorTheme exposing (..)


type alias Point =
    { x : Float, y : Float }


add : Point -> Point -> Point
add p q =
    { x = p.x + q.x, y = p.y + q.y }


sub : Point -> Point -> Point
sub p q =
    { x = p.x - q.x, y = p.y - q.y }


mul : Float -> Point -> Point
mul s p =
    { x = s * p.x, y = s * p.y }


mix2Color : Int -> Color
mix2Color n =
    case modBy 2 n of
        0 ->
            Primary

        _ ->
            Secondary


mix3Color : Int -> Color
mix3Color n =
    case modBy 3 n of
        0 ->
            Primary

        1 ->
            Secondary

        _ ->
            Ternary


mix4Color : Int -> Color
mix4Color n =
    case modBy 4 n of
        0 ->
            Primary

        1 ->
            Secondary

        2 ->
            Ternary

        _ ->
            Quart


tripleOp : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tripleOp f ( x, y, z ) =
    ( f x, f y, f z )


zip3 : ( a, a, a ) -> ( b, b, b ) -> ( ( a, b ), ( a, b ), ( a, b ) )
zip3 ( a1, a2, a3 ) ( b1, b2, b3 ) =
    ( ( a1, b1 ), ( a2, b2 ), ( a3, b3 ) )
