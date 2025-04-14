module Lab exposing (convexHexaTiling, floretHexaTiling, pythagoreanTiling)

import ColorTheme exposing (..)
import List exposing (repeat)
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
                (case modBy 2 m of
                    0 ->
                        add (floret |> setRotation -120 |> getPoint 2) (floret |> setRotation -120 |> getPoint 1)

                    _ ->
                        add (getPoint 3 floret) (getPoint 4 floret)
                )
                    |> mul size
                    |> add origin

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
                add
                    (getPoint 1 floret |> mul 2)
                    { x = 2, y = 0 }
                    |> mul size
                    |> add origin
        in
        renderShape floretHexaShape size origin theme ((mix3Color n |> repeat 6) ++ [ Quart ]) ++ floretHexaLine theme (n - 1) next_origin size


floretHexaShape : Shape
floretHexaShape =
    [ floret
    , floret |> setRotation 60
    , floret |> setRotation 120
    , floret |> setRotation 180
    , floret |> setRotation 240
    , floret |> setRotation 300
    , hexagon |> setOrigin (floret |> setRotation 60 |> getPoint 3)
    ]
        |> asShape


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
                    |> List.concatMap (\( point, size, color ) -> renderShape (asShape [ square ]) size point theme [ color ])
            )
        |> List.concat


{-| Convex Hexagon type 3 Tiling
-}
convexHexaTiling : Theme -> Int -> Int -> Point -> List (Svg msg)
convexHexaTiling theme n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                15

            next_origin =
                sub
                    (getPoint 2 convexHexa)
                    (convexHexa |> addRotation 120 |> getPoint 2)
                    |> mul size
                    |> add origin

            offset =
                case modBy 3 m of
                    0 ->
                        2

                    1 ->
                        1

                    _ ->
                        0
        in
        convexHexaLine theme n offset origin size ++ convexHexaTiling theme n (m - 1) next_origin


convexHexaLine : Theme -> Int -> Int -> Point -> Float -> List (Svg msg)
convexHexaLine theme n offset origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                sub
                    (getPoint 2 convexHexa)
                    (convexHexa |> addRotation 240 |> getPoint 2)
                    |> mul size
                    |> add origin
        in
        renderShape convexHexaShape size origin theme (n + offset |> mix3Color |> repeat 3) ++ convexHexaLine theme (n - 1) offset next_origin size


convexHexaShape : Shape
convexHexaShape =
    [ convexHexa
    , convexHexa |> addRotation 120
    , convexHexa |> addRotation 240
    ]
        |> asShape
