module Polygon exposing (..)

import ColorTheme exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


type alias Polygon =
    { lengths : List Float
    , angles : List Float
    , rotation : Float
    , origin : Point
    }


startAt : Polygon -> Int -> Polygon
startAt { lengths, angles, rotation, origin } n =
    { lengths = List.drop n lengths ++ List.take n lengths
    , angles = List.drop n angles ++ List.take n angles
    , rotation = rotation
    , origin = origin
    }


rotatePoly : Polygon -> Float -> Polygon
rotatePoly { lengths, angles, origin } a =
    { lengths = lengths
    , angles = angles
    , rotation = a
    , origin = origin
    }


addRotation : Polygon -> Float -> Polygon
addRotation { lengths, angles, rotation, origin } a =
    { lengths = lengths
    , angles = angles
    , rotation = rotation + a
    , origin = origin
    }


asPoints : Polygon -> List Point
asPoints { lengths, angles, rotation, origin } =
    List.map2 Tuple.pair lengths (rotation + 180 :: angles)
        |> pointSequence { x = 0, y = 0 } 0
        |> List.map (add origin)


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


drawAt : Point -> List Point -> List Point
drawAt origin points =
    points |> List.map (add origin)


scaleWith : Float -> List Point -> List Point
scaleWith size points =
    points |> List.map (\p -> mul p size)


polygonSvg : Polygon -> Float -> Point -> Theme -> Color -> Svg msg
polygonSvg poly size origin theme color =
    let
        svgPoints =
            asPoints poly
                |> scaleWith size
                |> drawAt origin
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
        asPoints poly |> scaleWith size |> List.drop (modBy n (n + index)) |> List.head
    of
        Nothing ->
            { x = 0, y = 0 }

        Just point ->
            point
