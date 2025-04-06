module Semiregular exposing (rhombiTriHexagonalTiling, triHexagonalTiling, truncatedHexagonalTiling, truncatedSquareTiling)

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
                if modBy 2 m == 0 then
                    add origin (getPoint dodecagon size 7)

                else
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
                if modBy 2 m == 0 then
                    add origin (getPoint hexagon size 3)

                else
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


{-| Rectification of the trihexagonal tiling

  - Type: semiregular
  - Corners: **3.4.6.4**
  - Symmetry: hexagonal

-}
rhombiTriHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
rhombiTriHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_point =
                if modBy 2 m == 0 then
                    sub (add (getPoint hexagon size -1) (getPoint (rotatePoly square -60) size -1)) { x = size, y = 0 }

                else
                    add (getPoint hexagon size 2) (getPoint (rotatePoly square -30) size 1)

            next_origin =
                add origin next_point
        in
        rhombiTriHexaLine n origin size ++ rhombiTriHexagonalTiling n (m - 1) next_origin


rhombiTriHexaLine : Int -> Point -> Float -> List (Svg msg)
rhombiTriHexaLine n origin size =
    if n <= 0 then
        []

    else
        let
            hexagon_width =
                sub (getPoint hexagon size 2) (getPoint hexagon size -1)

            square_slant =
                { x =
                    (sub (getPoint (rotatePoly square -30) size 1) (getPoint (rotatePoly square -30) size 0)).x
                , y = 0
                }

            next_origin =
                add origin (add hexagon_width (add square_slant (add { x = size, y = 0 } square_slant)))
        in
        rhombiTriHexaShape origin size ++ rhombiTriHexaLine (n - 1) next_origin size


rhombiTriHexaShape : Point -> Float -> List (Svg msg)
rhombiTriHexaShape origin size =
    let
        square1 =
            getPoint hexagon size 2

        trig1 =
            getPoint hexagon size 3

        square2 =
            getPoint hexagon size 4

        trig2 =
            getPoint hexagon size 4

        square3 =
            getPoint hexagon size 5
    in
    [ polygonSvg hexagon size Primary origin
    , polygonSvg (rotatePoly square -30) size Ternary (add origin square1)
    , polygonSvg (rotatePoly equilateral -30) size Secondary (add origin trig1)
    , polygonSvg square size Ternary (add origin square2)
    , polygonSvg (rotatePoly equilateral -90) size Secondary (add origin trig2)
    , polygonSvg (rotatePoly square -60) size Ternary (add origin square3)
    ]
