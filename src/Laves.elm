module Laves exposing (cairoTiling, deltoidalTriHexagonalTiling, disdyakisRhombileTiling, floretPentagonalTiling, prismaticPentagonalTiling, rhombileTiling, tetrakisSquareTiling, triakisTriangularTiling)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Shapes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


{-| Dual truncation of the triangular tiling

  - Type: laves
  - Symmetry: hexagonal

-}
triakisTriangularTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
triakisTriangularTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            downSlope =
                sub
                    (getPoint (rotatePoly obtuseIso -90) size 1)
                    (getPoint (rotatePoly obtuseIso -90) size 2)

            next_origin =
                if modBy 2 m == 0 then
                    add origin downSlope

                else
                    add origin { x = -downSlope.x, y = downSlope.y }
        in
        triakisTriLine theme n origin size ++ triakisTriangularTiling theme n (m - 1) next_origin


triakisTriLine : Theme -> Int -> Point -> Float -> List (Svg msg)
triakisTriLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            downSlant =
                sub (getPoint (rotatePoly obtuseIso 150) size 0)
                    (getPoint (rotatePoly obtuseIso 150) size 1)

            downOrigin =
                add origin downSlant

            upOrigin =
                add origin (add downSlant downSlant)

            next_origin =
                { x = origin.x + size * 2 * cos (degrees 30), y = origin.y }
        in
        triakisTriDownShape theme downOrigin size
            ++ triakisTriUpShape theme upOrigin size
            ++ triakisTriLine theme (n - 1) next_origin size


triakisTriDownShape : Theme -> Point -> Float -> List (Svg msg)
triakisTriDownShape theme origin size =
    [ polygonSvg (rotatePoly obtuseIso 30) size origin theme Secondary
    , polygonSvg (rotatePoly obtuseIso 150) size origin theme Primary
    , polygonSvg (rotatePoly obtuseIso -90) size origin theme Ternary
    ]


triakisTriUpShape : Theme -> Point -> Float -> List (Svg msg)
triakisTriUpShape theme origin size =
    [ polygonSvg (rotatePoly obtuseIso 90) size origin theme Secondary
    , polygonSvg (rotatePoly obtuseIso 210) size origin theme Ternary
    , polygonSvg (rotatePoly obtuseIso -30) size origin theme Primary
    ]


{-| Dual rectification of the triangular tiling

  - Type: laves
  - Symmetry: hexagonal

-}
rhombileTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
rhombileTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            downSlope =
                sub
                    (getPoint (rotatePoly rhombus 150) size 0)
                    (getPoint (rotatePoly rhombus 150) size 1)

            next_origin =
                add origin
                    (if modBy 2 m == 0 then
                        add downSlope { x = 0, y = size }

                     else
                        { x = -downSlope.x, y = downSlope.y + size }
                    )
        in
        rhombileLine theme n origin size ++ rhombileTiling theme n (m - 1) next_origin


rhombileLine : Theme -> Int -> Point -> Float -> List (Svg msg)
rhombileLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            upSlope =
                sub
                    (getPoint (rotatePoly rhombus 30) size 1)
                    (getPoint (rotatePoly rhombus 30) size 0)

            next_origin =
                add origin { x = upSlope.x * 2, y = 0 }
        in
        rhombileShape theme origin size ++ rhombileLine theme (n - 1) next_origin size


rhombileShape : Theme -> Point -> Float -> List (Svg msg)
rhombileShape theme origin size =
    [ polygonSvg (rotatePoly rhombus 30) size origin theme Secondary
    , polygonSvg (rotatePoly rhombus 150) size origin theme Ternary
    , polygonSvg (rotatePoly rhombus -90) size origin theme Primary
    ]


{-| Dual truncation of the square tiling

  - Type: laves
  - Symmetry: square

-}
tetrakisSquareTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
tetrakisSquareTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_origin =
                { x = origin.x, y = origin.y + size * 2 * sin (degrees 45) }
        in
        tetrakisSquareLine theme n origin size ++ tetrakisSquareTiling theme n (m - 1) next_origin


tetrakisSquareLine : Theme -> Int -> Point -> Float -> List (Svg msg)
tetrakisSquareLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + size * 2 * sin (degrees 45), y = origin.y }
        in
        tetrakisSquareShape theme origin size ++ tetrakisSquareLine theme (n - 1) next_origin size


tetrakisSquareShape : Theme -> Point -> Float -> List (Svg msg)
tetrakisSquareShape theme origin size =
    [ polygonSvg (rotatePoly isosceles 45) size origin theme Primary
    , polygonSvg (rotatePoly isosceles 135) size origin theme Ternary
    , polygonSvg (rotatePoly isosceles -135) size origin theme Secondary
    , polygonSvg (rotatePoly isosceles -45) size origin theme Quart
    ]


{-| Dual truncation of the rhombile tiling

  - Type: laves
  - Symmetry: hexagonal

-}
disdyakisRhombileTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
disdyakisRhombileTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                40

            slant =
                if modBy 2 m == 0 then
                    getPoint (rotatePoly left 330) size 2

                else
                    getPoint (rotatePoly left 270) size 2

            next_origin =
                add origin (add slant slant)
        in
        disdyakisRhombileLine theme n origin size ++ disdyakisRhombileTiling theme n (m - 1) next_origin


disdyakisRhombileLine : Theme -> Int -> Point -> Float -> List (Svg msg)
disdyakisRhombileLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + size * 2 * cos (degrees 30), y = origin.y }
        in
        disdyakisRhombileShape theme origin size ++ disdyakisRhombileLine theme (n - 1) next_origin size


disdyakisRhombileShape : Theme -> Point -> Float -> List (Svg msg)
disdyakisRhombileShape theme origin size =
    [ polygonSvg (startAt right 1) size origin theme Secondary
    , polygonSvg (rotatePoly left 30) size origin theme Primary
    , polygonSvg (rotatePoly (startAt right 1) 60) size origin theme Secondary
    , polygonSvg (rotatePoly left 90) size origin theme Primary
    , polygonSvg (rotatePoly (startAt right 1) 120) size origin theme Secondary
    , polygonSvg (rotatePoly left 150) size origin theme Primary
    , polygonSvg (rotatePoly (startAt right 1) 180) size origin theme Secondary
    , polygonSvg (rotatePoly left 210) size origin theme Primary
    , polygonSvg (rotatePoly (startAt right 1) 240) size origin theme Secondary
    , polygonSvg (rotatePoly left 270) size origin theme Primary
    , polygonSvg (rotatePoly (startAt right 1) 300) size origin theme Secondary
    , polygonSvg (rotatePoly left 330) size origin theme Primary
    ]


{-| Dual rectification of the rhombile tiling

  - Type: laves
  - Symmetry: hexagonal

-}
deltoidalTriHexagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
deltoidalTriHexagonalTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_origin =
                add origin
                    { x = size * cos (degrees 30) * toFloat (1 - modBy 2 m * 2)
                    , y = size + size * cos (degrees 60)
                    }

            color_offset =
                case modBy 2 m of
                    0 ->
                        1

                    _ ->
                        -1
        in
        deltoidalTriHexagonalLine theme n origin size ++ deltoidalTriHexagonalTiling theme (n + color_offset) (m - 1) next_origin


deltoidalTriHexagonalLine : Theme -> Int -> Point -> Float -> List (Svg msg)
deltoidalTriHexagonalLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + size * 2 * cos (degrees 30), y = origin.y }
        in
        deltoidalTriHexagonalShape theme (mix3Color n) origin size ++ deltoidalTriHexagonalLine theme (n - 1) next_origin size


deltoidalTriHexagonalShape : Theme -> Color -> Point -> Float -> List (Svg msg)
deltoidalTriHexagonalShape theme color origin size =
    [ polygonSvg kite size origin theme color
    , polygonSvg (rotatePoly kite 60) size origin theme color
    , polygonSvg (rotatePoly kite 120) size origin theme color
    , polygonSvg (rotatePoly kite 180) size origin theme color
    , polygonSvg (rotatePoly kite 240) size origin theme color
    , polygonSvg (rotatePoly kite 300) size origin theme color
    ]


{-| Half truncation of the disdyakis trihexagonal tiling

  - Type: laves
  - Symmetry: hex twist

-}
floretPentagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
floretPentagonalTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                15

            next_origin =
                add origin
                    (add
                        (getPoint (rotatePoly floret -60) size 2)
                        (getPoint (rotatePoly floret -60) size 1)
                    )

            color_offset =
                case modBy 3 m of
                    0 ->
                        0

                    1 ->
                        2

                    _ ->
                        1
        in
        floretPentagonalLine theme n color_offset origin size ++ floretPentagonalTiling theme n (m - 1) next_origin


floretPentagonalLine : Theme -> Int -> Int -> Point -> Float -> List (Svg msg)
floretPentagonalLine theme n offset origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                add origin
                    (add
                        (getPoint floret size 2)
                        (getPoint floret size 1)
                    )
        in
        floretPentagonalShape theme (mix3Color (n + offset)) origin size ++ floretPentagonalLine theme (n - 1) offset next_origin size


floretPentagonalShape : Theme -> Color -> Point -> Float -> List (Svg msg)
floretPentagonalShape theme color origin size =
    [ polygonSvg floret size origin theme color
    , polygonSvg (rotatePoly floret 60) size origin theme color
    , polygonSvg (rotatePoly floret 120) size origin theme color
    , polygonSvg (rotatePoly floret 180) size origin theme color
    , polygonSvg (rotatePoly floret 240) size origin theme color
    , polygonSvg (rotatePoly floret 300) size origin theme color
    ]


{-| Cairo Tiling

  - Type: laves
  - Symmetry: crosshatch

-}
cairoTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
cairoTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            downSlope =
                mul
                    (sub
                        (getPoint cairo size 3)
                        (getPoint cairo size 4)
                    )
                    2

            w =
                (getPoint cairo size 1).x

            next_origin =
                add
                    origin
                    (case modBy 2 m of
                        0 ->
                            { x = downSlope.x, y = w + downSlope.y }

                        _ ->
                            { x = -downSlope.x, y = w + downSlope.y }
                    )
        in
        cairoLine theme n origin size ++ cairoTiling theme n (m - 1) next_origin


cairoLine : Theme -> Int -> Point -> Float -> List (Svg msg)
cairoLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            w1 =
                (getPoint
                    (rotatePoly (startAt cairo 1) 30)
                    size
                    2
                ).x

            w2 =
                (getPoint cairo size 1).x

            next_origin =
                { x = origin.x + 2 * w1 + w2, y = origin.y }
        in
        cairoShape theme origin size ++ cairoLine theme (n - 1) next_origin size


cairoShape : Theme -> Point -> Float -> List (Svg msg)
cairoShape theme origin size =
    let
        tip =
            add origin
                (getPoint
                    (rotatePoly (startAt cairo 1) 30)
                    size
                    2
                )

        tip2 =
            add tip (getPoint cairo size 1)
    in
    [ polygonSvg (rotatePoly (startAt cairo 1) 30) size origin theme Primary
    , polygonSvg cairo size tip theme Secondary
    , polygonSvg (rotatePoly (startAt cairo 1) 120) size tip theme Secondary
    , polygonSvg (rotatePoly (startAt cairo 3) 60) size tip2 theme Primary
    ]


{-| Dual of the elongated triangular tiling

  - Type: laves
  - Symmetry: running bond

-}
prismaticPentagonalTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
prismaticPentagonalTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            leftSlope =
                sub
                    (getPoint prism size 3)
                    (getPoint prism size 4)

            rightSlope =
                sub
                    (getPoint prism size 3)
                    (getPoint prism size 2)

            next_origin =
                add (add origin { x = 0, y = 2 * size })
                    (case modBy 2 m of
                        0 ->
                            leftSlope

                        _ ->
                            rightSlope
                    )
        in
        prismaticPentagonalLine theme n origin size ++ prismaticPentagonalTiling theme n (m - 1) next_origin


prismaticPentagonalLine : Theme -> Int -> Point -> Float -> List (Svg msg)
prismaticPentagonalLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + size, y = origin.y }
        in
        prismaticPentagonalShape theme origin size ++ prismaticPentagonalLine theme (n - 1) next_origin size


prismaticPentagonalShape : Theme -> Point -> Float -> List (Svg msg)
prismaticPentagonalShape theme origin size =
    [ polygonSvg prism size origin theme Primary
    , polygonSvg (rotatePoly (startAt prism 2) 30) size (add origin (getPoint prism size 3)) theme Secondary
    ]
