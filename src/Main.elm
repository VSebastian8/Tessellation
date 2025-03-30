module Main exposing (main)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Regular exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)


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
            [ viewBox "100 100 900 900"
            , width "800"
            , height "800"
            ]
            (hexagons 100 100)
        ]
