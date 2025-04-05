module Semiregular exposing (..)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


dodecagon : Polygon
dodecagon =
    { lengths = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
    , angles = [ 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150 ]
    , rotation = 30
    }


equilateral : Polygon
equilateral =
    { lengths = [ 1, 1, 1 ]
    , angles = [ 60, 60, 60 ]
    , rotation = 0
    }


truncatedHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
truncatedHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            decagon_width =
                sub (getPoint dodecagon size 3) (getPoint dodecagon size 0)

            size =
                20

            next_origin =
                sub (add origin (getPoint dodecagon size 8)) decagon_width
        in
        truncatedHexagonalLine n origin ++ truncatedHexagonalTiling n (m - 1) next_origin


truncatedHexagonalLine : Int -> Point -> List (Svg msg)
truncatedHexagonalLine n origin =
    if n <= 0 then
        []

    else
        let
            size =
                20

            first_trig =
                getPoint dodecagon size 3

            second_trig =
                getPoint dodecagon size 6

            next_origin =
                add (add origin first_trig)
                    (getPoint equilateral size 1)
        in
        [ polygonSvg dodecagon size Primary origin, polygonSvg equilateral size Secondary (add origin first_trig), polygonSvg (rotatePoly equilateral 60) size Secondary (add origin second_trig) ] ++ truncatedHexagonalLine (n - 1) next_origin
