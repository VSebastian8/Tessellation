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


honeyTheme : Theme
honeyTheme =
    { strokeColor = "#CE7C00"
    , getColor =
        \color ->
            case color of
                Primary ->
                    "#FF9A0A"

                Secondary ->
                    "#FECB09"

                Ternary ->
                    "#FEED0D"

                Quart ->
                    "#CE7C00"
    }


forestTheme : Theme
forestTheme =
    { strokeColor = "#3E3F5B"
    , getColor =
        \color ->
            case color of
                Primary ->
                    "#8AB2A6"

                Secondary ->
                    "#ACD3A8"

                Ternary ->
                    "#F6F1DE"

                Quart ->
                    "#3E3F5B"
    }
