module ColorTheme exposing (..)


type Color
    = Primary
    | Secondary
    | Ternary
    | Quart
    | Stroke


type alias Theme =
    { getColor : Color -> String }


amethystTheme : Theme
amethystTheme =
    { getColor =
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

                Stroke ->
                    "black"
    }


aquaTheme : Theme
aquaTheme =
    { getColor =
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

                Stroke ->
                    "#006A71"
    }


honeyTheme : Theme
honeyTheme =
    { getColor =
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

                Stroke ->
                    "#CE7C00"
    }


forestTheme : Theme
forestTheme =
    { getColor =
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

                Stroke ->
                    "#3E3F5B"
    }
