module Regular exposing (hexagons, squares, triangles)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


{-| Square svg shape.
-}
square : Float -> Color -> Point -> Svg msg
square length color p =
    rect
        [ x (String.fromFloat p.x)
        , y (String.fromFloat p.y)
        , width (String.fromFloat length)
        , height (String.fromFloat length)
        , fill (theme.getColor color)
        , stroke theme.strokeColor
        , strokeWidth "2"
        ]
        []


{-| Regular Tiling of the plane with the `square` shape.
-}
squares : Int -> Int -> List (Svg msg)
squares n m =
    List.range 1 m
        |> List.map
            (\y ->
                List.range 1 n
                    |> List.map (\x -> ( { x = 20.0 * toFloat x, y = 20.0 * toFloat y }, mix2Color (x // 2 + y // 2) ))
                    |> List.map (\( p, c ) -> square 20 c p)
            )
        |> List.concat


{-| Square triangle shape pointing up.
-}
upTriangle : Float -> Color -> Point -> Svg msg
upTriangle length color p =
    polygon
        [ points
            (String.fromFloat (p.x + length / 100 * 50)
                ++ " "
                ++ String.fromFloat
                    (p.y + length / 100)
                ++ ", "
                ++ String.fromFloat (p.x + length)
                ++ " "
                ++ String.fromFloat
                    (p.y + length / 100 * 85)
                ++ ", "
                ++ String.fromFloat p.x
                ++ " "
                ++ String.fromFloat
                    (p.y + length / 100 * 85)
            )
        , fill (theme.getColor color)
        , stroke theme.strokeColor
        , strokeWidth "2"
        ]
        []


{-| Square triangle shape pointing down.
-}
downTriangle : Float -> Color -> Point -> Svg msg
downTriangle length color p =
    polygon
        [ points
            (String.fromFloat (p.x + length / 100 * 50)
                ++ " "
                ++ String.fromFloat
                    (p.y + length / 100 * 85)
                ++ ", "
                ++ String.fromFloat (p.x + length)
                ++ " "
                ++ String.fromFloat
                    p.y
                ++ ", "
                ++ String.fromFloat p.x
                ++ " "
                ++ String.fromFloat
                    p.y
            )
        , fill (theme.getColor color)
        , stroke theme.strokeColor
        , strokeWidth "2"
        ]
        []


{-| Regular Tiling of the plane with the `triangle` shape.
-}
triangles : Int -> Int -> List (Svg msg)
triangles n m =
    List.range 1 m
        |> List.map
            (\y ->
                List.range 1 n
                    |> List.map (\x -> ( { x = 30.0 * toFloat x + 15.0 * toFloat (modBy 2 (y + 1)), y = (sqrt 3 * 15.0) * toFloat (y // 2) }, mix2Color y ))
                    |> List.map
                        (\( p, c ) ->
                            case modBy 2 y of
                                0 ->
                                    downTriangle 30 c p

                                _ ->
                                    upTriangle 30 c p
                        )
            )
        |> List.concat


{-| Hexagon svg shape.
-}
hexagon : Float -> Color -> Point -> Svg msg
hexagon length color { x, y } =
    let
        height =
            length * 2

        width =
            length * sqrt 3

        -- Hexagon points (clockwise from top point)
        pointsHex =
            [ ( x, y - height / 2 ) -- Top center
            , ( x + width / 2, y - length / 2 ) -- Upper right
            , ( x + width / 2, y + length / 2 ) -- Lower right
            , ( x, y + height / 2 ) -- Bottom center
            , ( x - width / 2, y + length / 2 ) -- Lower left
            , ( x - width / 2, y - length / 2 ) -- Upper left
            ]
                |> List.map (\( px, py ) -> String.fromFloat px ++ "," ++ String.fromFloat py)
                |> String.join " "
    in
    polygon
        [ points pointsHex
        , fill (theme.getColor color)
        , stroke theme.strokeColor
        , strokeWidth "2"
        ]
        []


{-| Regular Tiling of the plane with the `hexagon` shape.
-}
hexagons : Int -> Int -> List (Svg msg)
hexagons n m =
    List.range 1 m
        |> List.map
            (\y ->
                List.range 1 n
                    |> List.map (\x -> ( { x = 20.0 * sqrt 3 * toFloat x + 10.0 * sqrt 3 * toFloat (modBy 2 (y + 1)), y = 30.0 * toFloat y }, mix3Color (x // 2 + y // 2) ))
                    |> List.map
                        (\( p, c ) -> hexagon 20 c p)
            )
        |> List.concat
