module Test exposing (..)

import ColorTheme exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Lab exposing (..)
import Laves exposing (..)
import Polygon exposing (..)
import Regular exposing (..)
import Semiregular exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import Util exposing (..)


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
            (convexHexaTiling
                forestTheme
                20
                10
                { x = 150, y = 150 }
            )
        ]


{-| Template Tiling

  - Type:
  - Symmetry:

-}
templateTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
templateTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_origin =
                origin
        in
        templateLine theme n origin size ++ templateTiling theme n (m - 1) next_origin


templateLine : Theme -> Int -> Point -> Float -> List (Svg msg)
templateLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                origin
        in
        templateShape theme origin size ++ templateLine theme (n - 1) next_origin size


templateShape : Theme -> Point -> Float -> List (Svg msg)
templateShape _ _ _ =
    []
