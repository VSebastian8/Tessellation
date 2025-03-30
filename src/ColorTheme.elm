module ColorTheme exposing (..)


type Color
    = Primary
    | Secondary
    | Ternary
    | Quart


type alias Theme =
    { strokeColor : String, getColor : Color -> String }


amethystTheme : Theme
amethystTheme =
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

                Quart ->
                    "#E9CDF6"
    }


aquaTheme : Theme
aquaTheme =
    { strokeColor = "#006A71"
    , getColor =
        \color ->
            case color of
                Primary ->
                    "#F2EFE7"

                Secondary ->
                    "#9ACBD0"

                Ternary ->
                    "#48A6A7"

                Quart ->
                    "#006A71"
    }


theme : Theme
theme =
    amethystTheme
