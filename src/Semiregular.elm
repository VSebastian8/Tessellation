module Semiregular exposing (elongatedTriangular, rhombiTriHexagonalTiling, snubSquareTiling, snubTriHexagonalTiling, triHexagonalTiling, truncatedHexagonalTiling, truncatedSquareTiling, truncatedTriHexagonalTiling)

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
truncatedHexagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
truncatedHexagonalTiling theme n m origin =
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
        truncatedHexagonalLine theme n origin ++ truncatedHexagonalTiling theme n (m - 1) next_origin


truncatedHexagonalLine : Theme -> Int -> Point -> List (Svg msg)
truncatedHexagonalLine theme n origin =
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
        [ polygonSvg dodecagon size theme Primary origin, polygonSvg equilateral size theme Secondary (add origin first_trig), polygonSvg (rotatePoly equilateral 60) size theme Secondary (add origin second_trig) ] ++ truncatedHexagonalLine theme (n - 1) next_origin


{-| Rectification of the hexagon (the original edges vanish)

  - Type: semiregular
  - Corners: **3.6.3.6**
  - Symmetry: hexagonal

-}
triHexagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
triHexagonalTiling theme n m origin =
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
        triHexagonalLine theme n origin ++ triHexagonalTiling theme n (m - 1) next_origin


triHexagonalLine : Theme -> Int -> Point -> List (Svg msg)
triHexagonalLine theme n origin =
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
        [ polygonSvg hexagon size theme Primary origin, polygonSvg equilateral size theme Secondary (add origin first_trig), polygonSvg (rotatePoly equilateral 60) size theme Secondary (add origin second_trig) ] ++ triHexagonalLine theme (n - 1) next_origin


{-| Truncation of the square tiling

  - Type: semiregular
  - Corners: **4.8.8**
  - Symmetry: square

-}
truncatedSquareTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
truncatedSquareTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            next_origin =
                add origin (getPoint octagon size 5)
        in
        truncatedSquareLine theme n origin ++ truncatedSquareTiling theme n (m - 1) next_origin


truncatedSquareLine : Theme -> Int -> Point -> List (Svg msg)
truncatedSquareLine theme n origin =
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
        [ polygonSvg octagon size theme Primary origin, polygonSvg (rotatePoly square 45) size theme Secondary (add origin top_square) ] ++ truncatedSquareLine theme (n - 1) next_origin


{-| Rectification of the trihexagonal tiling

  - Type: semiregular
  - Corners: **3.4.6.4**
  - Symmetry: hexagonal

-}
rhombiTriHexagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
rhombiTriHexagonalTiling theme n m origin =
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
        rhombiTriHexaLine theme n origin size ++ rhombiTriHexagonalTiling theme n (m - 1) next_origin


rhombiTriHexaLine : Theme -> Int -> Point -> Float -> List (Svg msg)
rhombiTriHexaLine theme n origin size =
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
        rhombiTriHexaShape theme origin size ++ rhombiTriHexaLine theme (n - 1) next_origin size


rhombiTriHexaShape : Theme -> Point -> Float -> List (Svg msg)
rhombiTriHexaShape theme origin size =
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
    [ polygonSvg hexagon size theme Primary origin
    , polygonSvg (rotatePoly square -30) size theme Ternary (add origin square1)
    , polygonSvg (rotatePoly equilateral -30) size theme Secondary (add origin trig1)
    , polygonSvg square size theme Ternary (add origin square2)
    , polygonSvg (rotatePoly equilateral -90) size theme Secondary (add origin trig2)
    , polygonSvg (rotatePoly square -60) size theme Ternary (add origin square3)
    ]


{-| Truncation of the trihexagonal tiling

  - Type: semiregular
  - Corners: **4.6.12**
  - Symmetry: hexagonal

-}
truncatedTriHexagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
truncatedTriHexagonalTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            hexagon_width =
                sub (getPoint (rotatePoly hexagon 30) size 2) (getPoint (rotatePoly hexagon 30) size 0)

            next_point =
                if modBy 2 m == 0 then
                    sub (add (getPoint dodecagon size 10) (getPoint (rotatePoly square -60) size -1)) (add hexagon_width { x = size, y = 0 })

                else
                    add (getPoint dodecagon size 6) (getPoint (rotatePoly square 60) size 2)

            next_origin =
                add origin next_point
        in
        truncTriHexaLine theme n origin size ++ truncatedTriHexagonalTiling theme n (m - 1) next_origin


truncTriHexaLine : Theme -> Int -> Point -> Float -> List (Svg msg)
truncTriHexaLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            dodecagon_width =
                sub (getPoint dodecagon size 4) (getPoint dodecagon size -1)

            hexagon_width =
                sub (getPoint (rotatePoly hexagon 30) size 2) (getPoint (rotatePoly hexagon 30) size 0)

            next_origin =
                add origin (add dodecagon_width (add hexagon_width (add hexagon_width { x = size, y = 0 })))
        in
        truncTriHexaShape theme origin size ++ truncTriHexaLine theme (n - 1) next_origin size


truncTriHexaShape : Theme -> Point -> Float -> List (Svg msg)
truncTriHexaShape theme origin size =
    let
        square1 =
            getPoint dodecagon size 6

        square2 =
            getPoint dodecagon size 8

        square3 =
            getPoint dodecagon size 10

        hex1 =
            getPoint dodecagon size 7

        hex2 =
            getPoint dodecagon size 9
    in
    [ polygonSvg dodecagon size theme Primary origin
    , polygonSvg (rotatePoly square 60) size theme Ternary (add origin square1)
    , polygonSvg square size theme Ternary (add origin square2)
    , polygonSvg (rotatePoly square -60) size theme Ternary (add origin square3)
    , polygonSvg (rotatePoly hexagon 30) size theme Secondary (add origin hex1)
    , polygonSvg (rotatePoly hexagon -30) size theme Secondary (add origin hex2)
    ]


{-| Half truncation of the truncated square tiling

  - Type: semiregular
  - Corners: **3.3.4.3.4**
  - Symmetry: crosshatch

-}
snubSquareTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
snubSquareTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            square_diag =
                sub (getPoint (rotatePoly square 30) size 2) (getPoint (rotatePoly square 30) size 0)

            next_origin =
                if modBy 2 m == 0 then
                    add origin (add square_diag { x = -2 * square_diag.x, y = size })

                else
                    add origin (add square_diag { x = 0, y = size })
        in
        snubSquareLine theme n origin size ++ snubSquareTiling theme n (m - 1) next_origin


snubSquareLine : Theme -> Int -> Point -> Float -> List (Svg msg)
snubSquareLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            triangle_slant =
                sub (getPoint (rotatePoly equilateral 90) size 2) (getPoint (rotatePoly equilateral 90) size 0)

            next_origin =
                { x = origin.x + 2 * triangle_slant.x + size, y = origin.y }
        in
        snubSquareShape theme origin size ++ snubSquareLine theme (n - 1) next_origin size


snubSquareShape : Theme -> Point -> Float -> List (Svg msg)
snubSquareShape theme origin size =
    let
        first =
            add origin (getPoint (rotatePoly equilateral 90) size 2)

        second =
            add first { x = size, y = 0 }
    in
    [ polygonSvg (rotatePoly equilateral 90) size theme Primary origin
    , polygonSvg (rotatePoly square 30) size theme Secondary origin
    , polygonSvg equilateral size theme Primary first
    , polygonSvg (rotatePoly equilateral 60) size theme Primary first
    , polygonSvg (rotatePoly equilateral 30) size theme Primary second
    , polygonSvg (rotatePoly square -30) size theme Secondary second
    ]


{-| Half truncation of the trihexagonal tiling

  - Type: semiregular
  - Corners: **3.3.3.3.6**
  - Symmetry: hex twist

-}
snubTriHexagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
snubTriHexagonalTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_origin =
                add origin
                    (case modBy 3 m of
                        0 ->
                            sub (getPoint hexagon size -1) { x = 2 * size, y = 0 }

                        1 ->
                            add (getPoint hexagon size 2) { x = 3 * size, y = 0 }

                        _ ->
                            sub (getPoint hexagon size -1) { x = 2 * size, y = 0 }
                    )
        in
        snubTriHexagonalLine theme n origin size ++ snubTriHexagonalTiling theme n (m - 1) next_origin


snubTriHexagonalLine : Theme -> Int -> Point -> Float -> List (Svg msg)
snubTriHexagonalLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + 7 * size, y = origin.y }
        in
        snubTriHexagonalShape theme origin size ++ snubTriHexagonalLine theme (n - 1) next_origin size


snubTriHexagonalShape : Theme -> Point -> Float -> List (Svg msg)
snubTriHexagonalShape theme origin size =
    let
        first =
            add origin (getPoint hexagon size 3)

        second =
            add origin (getPoint hexagon size 4)

        third =
            add origin (getPoint hexagon size 5)
    in
    [ polygonSvg hexagon size theme Primary origin
    , polygonSvg (rotatePoly equilateral 60) size theme Secondary first
    , polygonSvg equilateral size theme Secondary first
    , polygonSvg (rotatePoly equilateral -60) size theme Secondary first
    , polygonSvg equilateral size theme Secondary second
    , polygonSvg (rotatePoly equilateral -60) size theme Secondary second
    , polygonSvg (rotatePoly equilateral -120) size theme Secondary second
    , polygonSvg (rotatePoly equilateral -60) size theme Secondary third
    , polygonSvg (rotatePoly equilateral -120) size theme Secondary third
    ]


{-| Only non Wythoffian semiregular tiling

  - Type: semiregular
  - Corners: **3.3.3.4.4**
  - Symmetry: running bond

-}
elongatedTriangular : Theme -> Int -> Int -> Point -> List (Svg msg)
elongatedTriangular theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            squareLine =
                List.range 1 n |> List.map (\i -> polygonSvg square size theme Secondary { x = origin.x + toFloat i * size, y = origin.y })

            triangleLine =
                List.range 1 n
                    |> List.concatMap
                        (\i ->
                            [ polygonSvg (rotatePoly equilateral -60) size theme Primary { x = origin.x + toFloat i * size, y = origin.y }
                            , polygonSvg equilateral size theme Ternary { x = origin.x + toFloat i * size, y = origin.y }
                            ]
                        )

            selectedLine =
                if modBy 2 m == 0 then
                    squareLine

                else
                    triangleLine

            next_origin =
                add origin
                    (case modBy 4 m of
                        0 ->
                            { x = 0, y = size }

                        1 ->
                            getPoint (rotatePoly equilateral -60) size 1

                        2 ->
                            { x = 0, y = size }

                        _ ->
                            getPoint (rotatePoly equilateral -60) size -1
                    )
        in
        selectedLine ++ elongatedTriangular theme n (m - 1) next_origin
