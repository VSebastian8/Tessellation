module Main exposing (main)

import ColorTheme exposing (Color(..))
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Regular exposing (..)
import Semiregular exposing (..)
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
            [ viewBox "0 0 800 800"
            , width "800"
            , height "800"
            ]
            (triHexagonalTiling
                100
                100
                { x = -100, y = -100 }
            )
        ]
