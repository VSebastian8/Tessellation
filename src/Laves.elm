module Laves exposing (cairoTiling, deltoidalTriHexagonalTiling, disdyakisRhombileTiling, floretPentagonalTiling, prismaticPentagonalTiling, rhombileTiling, tetrakisSquareTiling, triakisTriangularTiling)

import ColorTheme exposing (..)
import List exposing (repeat)
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
                    (obtuseIso |> setRotation -90 |> getPoint 1)
                    (obtuseIso |> setRotation -90 |> getPoint 2)

            next_origin =
                (if modBy 2 m == 0 then
                    downSlope

                 else
                    { x = -downSlope.x, y = downSlope.y }
                )
                    |> mul size
                    |> add origin
        in
        triakisTriLine theme n origin size ++ triakisTriangularTiling theme n (m - 1) next_origin


triakisTriLine : Theme -> Int -> Point -> Float -> List (Svg msg)
triakisTriLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            downSlant =
                sub (obtuseIso |> setRotation 150 |> getPoint 0)
                    (obtuseIso |> setRotation 150 |> getPoint 1)

            downOrigin =
                downSlant |> mul size |> add origin

            upOrigin =
                add downSlant downSlant |> mul size |> add origin

            next_origin =
                { x = origin.x + size * 2 * cos (degrees 30), y = origin.y }
        in
        renderShape triakisTriDownShape size downOrigin theme [ Secondary, Primary, Ternary ]
            ++ renderShape triakisTriUpShape size upOrigin theme [ Secondary, Ternary, Primary ]
            ++ triakisTriLine theme (n - 1) next_origin size


triakisTriDownShape : Shape
triakisTriDownShape =
    [ setRotation 30 obtuseIso
    , setRotation 150 obtuseIso
    , setRotation -90 obtuseIso
    ]
        |> asShape


triakisTriUpShape : Shape
triakisTriUpShape =
    [ setRotation 90 obtuseIso
    , setRotation 210 obtuseIso
    , setRotation -30 obtuseIso
    ]
        |> asShape


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
                    (rhombus |> setRotation 150 |> getPoint 0)
                    (rhombus |> setRotation 150 |> getPoint 1)

            next_origin =
                (if modBy 2 m == 0 then
                    add downSlope { x = 0, y = 1 }

                 else
                    { x = -downSlope.x, y = downSlope.y + 1 }
                )
                    |> mul size
                    |> add origin
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
                    (rhombus |> setRotation 30 |> getPoint 1)
                    (rhombus |> setRotation 30 |> getPoint 0)

            next_origin =
                add origin { x = upSlope.x * 2 * size, y = 0 }
        in
        renderShape rhombileShape size origin theme [ Secondary, Ternary, Primary ]
            ++ rhombileLine theme (n - 1) next_origin size


rhombileShape : Shape
rhombileShape =
    [ setRotation 30 rhombus
    , setRotation 150 rhombus
    , setRotation -90 rhombus
    ]
        |> asShape


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
        renderShape tetrakisSquareShape size origin theme [ Primary, Ternary, Secondary, Quart ]
            ++ tetrakisSquareLine theme (n - 1) next_origin size


tetrakisSquareShape : Shape
tetrakisSquareShape =
    [ setRotation 45 isosceles
    , setRotation 135 isosceles
    , setRotation -135 isosceles
    , setRotation -45 isosceles
    ]
        |> asShape


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
                    left |> setRotation 330 |> getPoint 2

                else
                    left |> setRotation 270 |> getPoint 2

            next_origin =
                add slant slant |> mul size |> add origin
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
        renderShape disdyakisRhombileShape
            size
            origin
            theme
            ([ Secondary, Primary ] |> repeat 6 |> List.concat)
            ++ disdyakisRhombileLine theme (n - 1) next_origin size


disdyakisRhombileShape : Shape
disdyakisRhombileShape =
    [ startAt 1 right
    , setRotation 30 left
    , right |> setRotation 60 |> startAt 1
    , setRotation 90 left
    , right |> setRotation 120 |> startAt 1
    , setRotation 150 left
    , right |> setRotation 180 |> startAt 1
    , setRotation 210 left
    , right |> setRotation 240 |> startAt 1
    , setRotation 270 left
    , right |> setRotation 300 |> startAt 1
    , setRotation 330 left
    ]
        |> asShape


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
        renderShape deltoidalTriHexagonalShape size origin theme (mix3Color n |> repeat 6)
            ++ deltoidalTriHexagonalLine theme (n - 1) next_origin size


deltoidalTriHexagonalShape : Shape
deltoidalTriHexagonalShape =
    [ kite
    , kite |> setRotation 60
    , kite |> setRotation 120
    , kite |> setRotation 180
    , kite |> setRotation 240
    , kite |> setRotation 300
    ]
        |> asShape


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
                add
                    (floret |> setRotation -60 |> getPoint 2)
                    (floret |> setRotation -60 |> getPoint 1)
                    |> mul size
                    |> add origin

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
                add
                    (getPoint 2 floret)
                    (getPoint 1 floret)
                    |> mul size
                    |> add origin
        in
        renderShape floretPentagonalShape size origin theme (mix3Color (n + offset) |> repeat 6)
            ++ floretPentagonalLine theme (n - 1) offset next_origin size


floretPentagonalShape : Shape
floretPentagonalShape =
    [ floret
    , floret |> setRotation 60
    , floret |> setRotation 120
    , floret |> setRotation 180
    , floret |> setRotation 240
    , floret |> setRotation 300
    ]
        |> asShape


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
                sub
                    (getPoint 3 cairo)
                    (getPoint 4 cairo)
                    |> mul 2

            w =
                (getPoint 1 cairo).x

            next_origin =
                (case modBy 2 m of
                    0 ->
                        { x = downSlope.x, y = w + downSlope.y }

                    _ ->
                        { x = -downSlope.x, y = w + downSlope.y }
                )
                    |> mul size
                    |> add origin
        in
        cairoLine theme n origin size ++ cairoTiling theme n (m - 1) next_origin


cairoLine : Theme -> Int -> Point -> Float -> List (Svg msg)
cairoLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            w1 =
                (cairo |> setRotation 30 |> startAt 1 |> getPoint 2).x * size

            w2 =
                (getPoint 1 cairo).x * size

            next_origin =
                { x = origin.x + 2 * w1 + w2, y = origin.y }
        in
        renderShape
            cairoShape
            size
            origin
            theme
            [ Primary, Secondary, Secondary, Primary ]
            ++ cairoLine theme (n - 1) next_origin size


cairoShape : Shape
cairoShape =
    let
        tip =
            cairo |> setRotation 30 |> startAt 1 |> getPoint 2

        tip2 =
            add tip (getPoint 1 cairo)
    in
    [ cairo |> setRotation 30 |> startAt 1
    , cairo |> setOrigin tip
    , cairo |> setRotation 120 |> startAt 1 |> setOrigin tip
    , cairo |> setRotation 60 |> startAt 3 |> setOrigin tip2
    ]
        |> asShape


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
                    (getPoint 3 prism)
                    (getPoint 4 prism)

            rightSlope =
                sub
                    (getPoint 3 prism)
                    (getPoint 2 prism)

            next_origin =
                (case modBy 2 m of
                    0 ->
                        leftSlope

                    _ ->
                        rightSlope
                )
                    |> add { x = 0, y = 2 }
                    |> mul size
                    |> add origin
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
        renderShape prismaticPentagonalShape size origin theme [ Primary, Secondary ]
            ++ prismaticPentagonalLine theme (n - 1) next_origin size


prismaticPentagonalShape : Shape
prismaticPentagonalShape =
    [ prism
    , prism |> setRotation 30 |> startAt 2 |> setOrigin (getPoint 3 prism)
    ]
        |> asShape
