module Test exposing (main)

import ColorTheme exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Polygon exposing (..)
import RuleBased.Isogonal exposing (..)
import RuleBased.Regular exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import String exposing (fromFloat)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Util exposing (..)


showTess : Tess -> List (Html msg)
showTess tess =
    let
        w =
            1200

        h =
            800
    in
    [ div
        [ style "border" "solid 5px "
        , style "padding-bottom" "0"
        , style "height" "fit-content"
        ]
        [ svg
            [ viewBox ("0 0 " ++ fromFloat w ++ " " ++ fromFloat h)
            , width (fromFloat w)
            , height (fromFloat h)
            , style "margin-bottom" "-5px"
            ]
            (renderTess (fix tess ( { x = -3, y = -3 }, { x = w / tess.size + 2, y = h / tess.size + 2 } )) forestTheme)
        ]
    , div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "padding" "0"
        , style "margin-left" "50px"
        , style "height" "800px" -- , style "background-color" "blue"
        ]
        (tess.rules
            |> List.map (\r -> renderRule r forestTheme)
        )
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
            hexaStarTessellation
        )
