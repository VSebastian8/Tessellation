module Test exposing (..)

import ColorTheme exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Laves exposing (..)
import Polygon exposing (..)
import Regular exposing (..)
import Semiregular exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)


theme : Theme
theme =
    forestTheme


main : Html msg
main =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" "100vh"
        , style "margin" "0"
        ]
        [ svg
            [ viewBox "0 0 800 800"
            , width "800"
            , height "800"
            ]
            (rhombileTiling
                theme
                20
                20
                { x = 150, y = 150 }
            )
        ]
