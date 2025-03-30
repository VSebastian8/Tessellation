module Main exposing (main)

import Html exposing (Html)
import Regular exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Html msg
main =
    svg
        [ viewBox "0 0 800 800"
        , width "800"
        , height "800"
        ]
        (hexagons 100 100)
