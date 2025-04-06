module Semiregular exposing (triHexagonalTiling, truncatedHexagonalTiling, truncatedSquareTiling)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Shapes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


{-| Truncating the corners of the hexagon (adding triangles at each corner)

  - Type: semiregular
  - Corners: **3.12.12**
  - Symmetry: hexagonal

-}
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


{-| Rectification of the hexagon (the original edges vanish)

  - Type: semiregular
  - Corners: **3.6.3.6**
  - Symmetry: hexagonal

-}
triHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
triHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            hexagon_translate =
                { x = size * 2, y = 0 }

            next_origin =
                sub (add origin (getPoint hexagon size 3)) hexagon_translate
        in
        triHexagonalLine n origin ++ triHexagonalTiling n (m - 1) next_origin


triHexagonalLine : Int -> Point -> List (Svg msg)
triHexagonalLine n origin =
    if n <= 0 then
        []

    else
        let
            size =
                30

            first_trig =
                getPoint hexagon size 1

            second_trig =
                getPoint hexagon size 3

            next_origin =
                add (add origin first_trig)
                    (getPoint equilateral size 1)
        in
        [ polygonSvg hexagon size Primary origin, polygonSvg equilateral size Secondary (add origin first_trig), polygonSvg (rotatePoly equilateral 60) size Secondary (add origin second_trig) ] ++ triHexagonalLine (n - 1) next_origin


{-| Truncation of the square tiling

  - Type: semiregular
  - Corners: **4.8.8**
  - Symmetry: square

-}
truncatedSquareTiling : Int -> Int -> Point -> List (Svg msg)
truncatedSquareTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            next_origin =
                add origin (getPoint octagon size 5)
        in
        truncatedSquareLine n origin ++ truncatedSquareTiling n (m - 1) next_origin


truncatedSquareLine : Int -> Point -> List (Svg msg)
truncatedSquareLine n origin =
    if n <= 0 then
        []

    else
        let
            size =
                20

            top_square =
                getPoint octagon size 1

            next_origin =
                add (add origin top_square)
                    (getPoint (rotatePoly square 45) size 2)
        in
        [ polygonSvg octagon size Primary origin, polygonSvg (rotatePoly square 45) size Secondary (add origin top_square) ] ++ truncatedSquareLine (n - 1) next_origin
