module Test exposing (..)

import ColorTheme exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Lab exposing (..)
import Laves exposing (..)
import Polygon exposing (..)
import Regular exposing (..)
import Rules exposing (..)
import Semiregular exposing (..)
import Shapes exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import Util exposing (..)


sq : PC
sq =
    { poly = square, col = Primary, centre = { x = 0.5, y = 0.5 }, dist = 0.5 }


eqi : PC
eqi =
    { poly = equilateral, col = Primary, centre = { x = 0.5, y = 0.28 }, dist = 0.28 }


squareRule1 : Rule
squareRule1 =
    { anchor = sq, additions = [ tr { x = 1, y = 0 } { sq | col = Secondary }, tr { x = 0, y = 1 } { eqi | col = Secondary } ] }


squareRule2 : Rule
squareRule2 =
    { anchor = { sq | col = Secondary }, additions = [ tr { x = 1, y = 0 } sq, eqi |> tr { x = 0, y = 1 } ] }


triangleRule1 : Rule
triangleRule1 =
    { anchor = { eqi | col = Secondary }
    , additions = [ { eqi | col = Ternary } |> rt { x = 0, y = 0 } -60 |> tr { x = 1, y = 0 }, { eqi | col = Ternary } |> rt { x = 0, y = 0 } -60, sq |> tr (equilateral |> getPoint 2) |> tr { x = -0.5, y = 0 } ]
    }


squareTessellation : Tess
squareTessellation =
    { rules =
        [ squareRule1
        , squareRule2
        , triangleRule1
        ]
    , open = [ sq ]
    , closed = []
    , bounds = ( { x = -1, y = -1 }, { x = 28, y = 28 } ) -- width / size, height / size
    }


showTess : Tess -> List (Html msg)
showTess tess =
    [ div
        [ style "border" "solid 5px "
        , style "padding-bottom" "0"
        , style "height" "fit-content"
        ]
        [ svg
            [ viewBox "0 0 800 800"
            , width "800"
            , height "800"
            , style "margin-bottom" "-5px"
            ]
            (renderTess (fix tess) 30 forestTheme)
        ]
    , svg
        [ viewBox "0 0 200 800"
        , width "400"
        , height "800"
        ]
        (tess.rules |> List.indexedMap (\i r -> renderRule r { x = 100, y = 50 + 100 * toFloat i } 30 forestTheme) |> List.concat)
    ]


main : Html msg
main =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" "100vh"
        , style "margin" "0"
        ]
        (showTess
            squareTessellation
        )


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
        renderShape templateShape size origin theme [ Primary ]
            ++ templateLine theme (n - 1) next_origin size


templateShape : Shape
templateShape =
    [] |> asShape
