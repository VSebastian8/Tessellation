module Test exposing (main, testTessellation)

-- import Hardcoded.Lab exposing (..)
-- import Hardcoded.Laves exposing (..)
-- import Hardcoded.Regular exposing (..)
-- import Hardcoded.Semiregular exposing (..)

import ColorTheme exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Polygon exposing (..)
import RuleBased.Isogonal exposing (..)
import RuleBased.Regular exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Util exposing (..)


testRule1 : Rule
testRule1 =
    { anchor = squ
    , additions = [ tr { x = 1, y = 0 } { squ | col = Secondary }, tr { x = 0, y = 1 } { eqi | col = Secondary } ]
    , rotatable = False
    }


testRule2 : Rule
testRule2 =
    { anchor = { squ | col = Secondary }
    , additions = [ tr { x = 1, y = 0 } squ, eqi |> tr { x = 0, y = 1 } ]
    , rotatable = False
    }


testRule3 : Rule
testRule3 =
    { anchor = { eqi | col = Secondary }
    , additions = [ { eqi | col = Ternary } |> rt { x = 0, y = 0 } -60 |> tr { x = 1, y = 0 }, { eqi | col = Ternary } |> rt { x = 0, y = 0 } -60, squ |> tr (equilateral |> getPoint 2) |> tr { x = -0.5, y = 0 } ]
    , rotatable = False
    }


testTessellation : Tess
testTessellation =
    { rules =
        [ testRule1
        , testRule2
        , testRule3
        ]
    , open = [ squ ]
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
            triangularRowsTessellation
        )
