module Polygon exposing (..)

import ColorTheme exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


type alias Polygon =
    { lengths : List Float
    , angles : List Float
    , rotation : Float
    }


startAt : Polygon -> Int -> Polygon
startAt { lengths, angles, rotation } n =
    { lengths = List.drop n lengths ++ List.take n lengths
    , angles = List.drop n angles ++ List.take n angles
    , rotation = rotation
    }


rotatePoly : Polygon -> Float -> Polygon
rotatePoly { lengths, angles } a =
    { lengths = lengths
    , angles = angles
    , rotation = a
    }


polygonPoints : Polygon -> Float -> List Point
polygonPoints { lengths, angles, rotation } size =
    List.map2 Tuple.pair (List.map (\l -> l * size) lengths) (rotation + 180 :: angles)
        |> pointSequence { x = 0, y = 0 } 0


pointSequence : Point -> Float -> List ( Float, Float ) -> List Point
pointSequence point rotation polyList =
    case polyList of
        [] ->
            []

        ( length, angle ) :: rest ->
            let
                next_rotation =
                    180 + rotation + angle

                next_point =
                    { x = point.x + length * cos (degrees next_rotation), y = point.y - length * sin (degrees next_rotation) }
            in
            point :: pointSequence next_point next_rotation rest


startPoint : List Point -> Point -> List Point
startPoint points p =
    points |> List.map (\q -> { x = q.x + p.x, y = q.y + p.y })


polygonSvg : Polygon -> Float -> Color -> Point -> Svg msg
polygonSvg poly size color point =
    let
        svgPoints =
            startPoint (polygonPoints poly size) point
                |> List.map (\p -> String.fromFloat p.x ++ "," ++ String.fromFloat p.y)
                |> String.join " "
    in
    polygon
        [ points svgPoints
        , fill (theme.getColor color)
        , stroke theme.strokeColor
        , strokeWidth "2"
        ]
        []


getPoint : Polygon -> Float -> Int -> Point
getPoint poly size index =
    let
        n =
            List.length poly.lengths
    in
    case
        polygonPoints poly size |> List.drop (modBy n (n + index)) |> List.head
    of
        Nothing ->
            { x = 0, y = 0 }

        Just point ->
            point
