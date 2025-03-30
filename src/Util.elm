module Util exposing (..)


type alias Point =
    { x : Float, y : Float }


type Color
    = Primary
    | Secondary
    | Ternary


type alias Theme =
    { strokeColor : String, getColor : Color -> String }


theme : Theme
theme =
    { strokeColor = "black"
    , getColor =
        \color ->
            case color of
                Primary ->
                    "#810EA9"

                Secondary ->
                    "#BC7AD2"

                Ternary ->
                    "#51185C"
    }


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
