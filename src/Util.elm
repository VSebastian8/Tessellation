module Util exposing (..)

import ColorTheme exposing (..)
import Svg exposing (Svg)


type alias Point =
    { x : Float, y : Float }


type alias Shape msg =
    { render : List (Svg msg), leftTop : Point, rightBottom : Point }


after : Point -> Point -> Bool
after p q =
    q.x > p.x || q.y > p.y


before : Point -> Point -> Bool
before p q =
    q.x < p.x || q.y < p.y


renderShape : Shape msg -> Point -> List (Svg msg)
renderShape { render, leftTop, rightBottom } origin =
    let
        left =
            add origin leftTop

        right =
            add origin rightBottom

        leftBound =
            { x = 0, y = 0 }

        rightBound =
            { x = 250, y = 500 }
    in
    if before leftBound right || after rightBound left then
        []

    else
        render


add : Point -> Point -> Point
add p q =
    { x = p.x + q.x, y = p.y + q.y }


sub : Point -> Point -> Point
sub p q =
    { x = p.x - q.x, y = p.y - q.y }


mul : Point -> Float -> Point
mul p s =
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
