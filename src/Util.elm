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
