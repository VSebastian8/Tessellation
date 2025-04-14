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


startAt : Int -> Polygon -> Polygon
startAt n { lengths, angles, rotation, origin } =
    { lengths = List.drop n lengths ++ List.take n lengths
    , angles = List.drop n angles ++ List.take n angles
    , rotation = rotation
    , origin = origin
    }


setRotation : Float -> Polygon -> Polygon
setRotation angle { lengths, angles, origin } =
    { lengths = lengths
    , angles = angles
    , rotation = angle
    , origin = origin
    }


addRotation : Float -> Polygon -> Polygon
addRotation angle { lengths, angles, rotation, origin } =
    { lengths = lengths
    , angles = angles
    , rotation = rotation + angle
    , origin = origin
    }


setOrigin : Point -> Polygon -> Polygon
setOrigin origin { lengths, angles, rotation } =
    { lengths = lengths
    , angles = angles
    , rotation = rotation
    , origin = origin
    }


asPoints : Polygon -> List Point
asPoints { lengths, angles, rotation, origin } =
    List.map2 Tuple.pair lengths (rotation + 180 :: angles)
        |> pointSequence { x = 0, y = 0 } 0
        |> List.map (add origin)


getPoint : Int -> Polygon -> Point
getPoint index poly =
    let
        n =
            List.length poly.lengths
    in
    case
        asPoints poly |> List.drop (modBy n (n + index)) |> List.head
    of
        Nothing ->
            { x = 0, y = 0 }

        Just point ->
            point


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
    points |> List.map (\p -> mul size p)


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
