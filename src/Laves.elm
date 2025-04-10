module Laves exposing (rhombileTiling, triakisTriangularTiling)

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
