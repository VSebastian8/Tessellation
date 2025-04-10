module Laves exposing (deltoidalTriHexagonalTiling, disdyakisRhombileTiling, floretPentagonalTiling, rhombileTiling, tetrakisSquareTiling, triakisTriangularTiling)

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
    [ polygonSvg (rotatePoly obtuseIso 30) size theme Secondary origin
    , polygonSvg (rotatePoly obtuseIso 150) size theme Primary origin
    , polygonSvg (rotatePoly obtuseIso -90) size theme Ternary origin
    ]


triakisTriUpShape : Theme -> Point -> Float -> List (Svg msg)
triakisTriUpShape theme origin size =
    [ polygonSvg (rotatePoly obtuseIso 90) size theme Secondary origin
    , polygonSvg (rotatePoly obtuseIso 210) size theme Ternary origin
    , polygonSvg (rotatePoly obtuseIso -30) size theme Primary origin
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
    [ polygonSvg (rotatePoly rhombus 30) size theme Secondary origin
    , polygonSvg (rotatePoly rhombus 150) size theme Ternary origin
    , polygonSvg (rotatePoly rhombus -90) size theme Primary origin
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
    [ polygonSvg (rotatePoly isosceles 45) size theme Primary origin
    , polygonSvg (rotatePoly isosceles 135) size theme Ternary origin
    , polygonSvg (rotatePoly isosceles -135) size theme Secondary origin
    , polygonSvg (rotatePoly isosceles -45) size theme Quart origin
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
    [ polygonSvg (startAt right 1) size theme Secondary origin
    , polygonSvg (rotatePoly left 30) size theme Primary origin
    , polygonSvg (rotatePoly (startAt right 1) 60) size theme Secondary origin
    , polygonSvg (rotatePoly left 90) size theme Primary origin
    , polygonSvg (rotatePoly (startAt right 1) 120) size theme Secondary origin
    , polygonSvg (rotatePoly left 150) size theme Primary origin
    , polygonSvg (rotatePoly (startAt right 1) 180) size theme Secondary origin
    , polygonSvg (rotatePoly left 210) size theme Primary origin
    , polygonSvg (rotatePoly (startAt right 1) 240) size theme Secondary origin
    , polygonSvg (rotatePoly left 270) size theme Primary origin
    , polygonSvg (rotatePoly (startAt right 1) 300) size theme Secondary origin
    , polygonSvg (rotatePoly left 330) size theme Primary origin
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
        in
        deltoidalTriHexagonalLine theme n origin size ++ deltoidalTriHexagonalTiling theme n (m - 1) next_origin


deltoidalTriHexagonalLine : Theme -> Int -> Point -> Float -> List (Svg msg)
deltoidalTriHexagonalLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + size * 2 * cos (degrees 30), y = origin.y }
        in
        deltoidalTriHexagonalShape theme origin size ++ deltoidalTriHexagonalLine theme (n - 1) next_origin size


deltoidalTriHexagonalShape : Theme -> Point -> Float -> List (Svg msg)
deltoidalTriHexagonalShape theme origin size =
    [ polygonSvg kite size theme Ternary origin
    , polygonSvg (rotatePoly kite 60) size theme Secondary origin
    , polygonSvg (rotatePoly kite 120) size theme Primary origin
    , polygonSvg (rotatePoly kite 180) size theme Ternary origin
    , polygonSvg (rotatePoly kite 240) size theme Secondary origin
    , polygonSvg (rotatePoly kite 300) size theme Primary origin
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
                10

            next_origin =
                add origin
                    (case modBy 3 m of
                        0 ->
                            add (getPoint (rotatePoly floret -120) size 2) (getPoint (rotatePoly floret -120) size 1)

                        _ ->
                            add (getPoint (rotatePoly floret -60) size 2) (getPoint (rotatePoly floret -60) size 1)
                    )
        in
        floretPentagonalLine theme n origin size ++ floretPentagonalTiling theme n (m - 1) next_origin


floretPentagonalLine : Theme -> Int -> Point -> Float -> List (Svg msg)
floretPentagonalLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                add origin
                    (case modBy 5 n of
                        4 ->
                            add (getPoint (rotatePoly floret 60) size 2) (getPoint (rotatePoly floret 60) size 1)

                        _ ->
                            add (getPoint floret size 2) (getPoint floret size 1)
                    )
        in
        floretPentagonalShape theme origin size ++ floretPentagonalLine theme (n - 1) next_origin size


floretPentagonalShape : Theme -> Point -> Float -> List (Svg msg)
floretPentagonalShape theme origin size =
    [ polygonSvg floret size theme Ternary origin
    , polygonSvg (rotatePoly floret 60) size theme Secondary origin
    , polygonSvg (rotatePoly floret 120) size theme Primary origin
    , polygonSvg (rotatePoly floret 180) size theme Ternary origin
    , polygonSvg (rotatePoly floret 240) size theme Secondary origin
    , polygonSvg (rotatePoly floret 300) size theme Primary origin
    ]
