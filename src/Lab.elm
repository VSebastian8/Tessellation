module Lab exposing (floretHexaTiling, pythagoreanTiling)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Shapes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


{-| Pentagonal floret and hexagon tessellation
-}
floretHexaTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
floretHexaTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                10

            next_origin =
                add origin
                    (case modBy 2 m of
                        0 ->
                            add (getPoint (rotatePoly floret -120) size 2) (getPoint (rotatePoly floret -120) size 1)

                        _ ->
                            add (getPoint floret size 3) (getPoint floret size 4)
                    )

            color_offset =
                case modBy 2 m of
                    0 ->
                        2

                    _ ->
                        -2
        in
        floretHexaLine theme n origin size ++ floretHexaTiling theme (n + color_offset) (m - 1) next_origin


floretHexaLine : Theme -> Int -> Point -> Float -> List (Svg msg)
floretHexaLine theme n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                add origin
                    (add
                        (mul (getPoint floret size 1) 2)
                        { x = 2 * size, y = 0 }
                    )
        in
        floretHexaShape theme (mix3Color n) origin size ++ floretHexaLine theme (n - 1) next_origin size


floretHexaShape : Theme -> Color -> Point -> Float -> List (Svg msg)
floretHexaShape theme color origin size =
    [ polygonSvg floret size theme color origin
    , polygonSvg (rotatePoly floret 60) size theme color origin
    , polygonSvg (rotatePoly floret 120) size theme color origin
    , polygonSvg (rotatePoly floret 180) size theme color origin
    , polygonSvg (rotatePoly floret 240) size theme color origin
    , polygonSvg (rotatePoly floret 300) size theme color origin
    , polygonSvg hexagon size theme Quart (add origin (getPoint (rotatePoly floret 60) size 3))
    ]


{-| Big squares with little squares between them
-}
pythagoreanTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
pythagoreanTiling theme n m origin =
    let
        big_size =
            30.0

        small_size =
            12.0

        diff =
            big_size - small_size
    in
    List.range 1 m
        |> List.map
            (\y ->
                List.range 1 n
                    |> List.concatMap
                        (\x ->
                            [ ( add origin
                                    { x = big_size * toFloat x + small_size * toFloat (x - 1) + small_size * toFloat y
                                    , y = big_size * toFloat y + diff * toFloat (x - 1)
                                    }
                              , big_size
                              , mix2Color y
                              )
                            , ( add origin
                                    { x = big_size * toFloat (x + 1) + small_size * toFloat y + small_size * toFloat (x - 1)
                                    , y = big_size * toFloat (y + 1) + diff * toFloat (x - 1) - small_size
                                    }
                              , small_size
                              , Ternary
                              )
                            ]
                        )
                    |> List.map (\( point, size, color ) -> polygonSvg square size theme color point)
            )
        |> List.concat
