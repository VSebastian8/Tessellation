module Rules exposing (..)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Shapes exposing (..)
import Svg exposing (Svg)
import Util exposing (..)


type alias PC =
    { poly : Polygon, col : Color, centre : Point, dist : Float }


collides : PC -> PC -> Bool
collides p1 p2 =
    distance p1.centre p2.centre < p1.dist + p2.dist - epsilon


type alias Rule =
    { anchor : PC, additions : List PC, rotatable : Bool }


eq : PC -> PC -> Bool
eq p1 p2 =
    equals p1.poly p2.poly && (p1.col == p2.col)


eq2 : PC -> PC -> Bool
eq2 p1 p2 =
    (p1.poly.lengths == p2.poly.lengths) && (p1.poly.angles == p2.poly.angles) && (p1.col == p2.col)


applies : Rule -> PC -> Bool
applies rule p =
    if rule.rotatable then
        eq2 rule.anchor p

    else
        eq rule.anchor p


tr : Point -> PC -> PC
tr p pc =
    { pc | poly = translate p pc.poly, centre = add pc.centre p }


rt : Point -> Float -> PC -> PC
rt origin angle pc =
    { pc | poly = pc.poly |> setRotation (angle + pc.poly.rotation) |> setOrigin (rotateAround origin angle pc.poly.origin), centre = pc.centre |> rotateAround origin angle }


sz : Float -> PC -> PC
sz size pc =
    let
        p =
            pc.poly
    in
    { pc | poly = { p | lengths = List.map (\l -> l * size) p.lengths }, centre = mul size pc.centre, dist = pc.dist * size }


renderRule : Rule -> Point -> Float -> Theme -> List (Svg msg)
renderRule { anchor, additions } at size theme =
    (additions
        |> List.concatMap
            (\addition ->
                [ polygonSvg addition.poly size at theme addition.col 2
                , pointSvg addition.centre size at 2
                ]
            )
    )
        ++ [ polygonSvg anchor.poly size at theme anchor.col 4, pointSvg anchor.centre size at 2 ]


type alias Tess =
    { rules : List Rule
    , open : List PC
    , closed : List PC
    , bounds : ( Point, Point )
    }


renderTess : Tess -> Float -> Theme -> List (Svg msg)
renderTess { closed } size theme =
    closed |> List.map (\p -> polygonSvg p.poly size { x = 0, y = 0 } theme p.col 2)


step : Tess -> Tess
step tess =
    case tess.open of
        [] ->
            tess

        -- Pick the first open polygon, check its validity and apply all rules to it
        p :: rest ->
            if not (inside p.poly.origin tess.bounds) || List.any (collides p) tess.closed then
                { tess
                    | open = rest
                }

            else
                let
                    new_ps =
                        tess.rules
                            |> List.filter (\rule -> applies rule p)
                            |> List.concatMap
                                (\rule ->
                                    rule.additions
                                        |> List.map
                                            (\p2 ->
                                                p2
                                                    |> rt rule.anchor.poly.origin
                                                        (p.poly.rotation
                                                            - rule.anchor.poly.rotation
                                                        )
                                                    |> tr p.poly.origin
                                                    |> tr (neg rule.anchor.poly.origin)
                                            )
                                )
                in
                { tess
                    | open = rest ++ new_ps
                    , closed = p :: tess.closed
                }


fix : Tess -> Tess
fix t =
    case t.open of
        [] ->
            { t | closed = List.reverse t.closed }

        _ ->
            fix (step t)


squ : PC
squ =
    { poly = square, col = Primary, centre = { x = 0.5, y = 0.5 }, dist = 0.5 }


eqi : PC
eqi =
    { poly = equilateral, col = Primary, centre = { x = 0.5, y = 0.28 }, dist = 0.28 }


hex : PC
hex =
    { poly = hexagon, col = Primary, centre = { x = 0.5, y = 0.86 }, dist = 0.86 } |> rt { x = 0, y = 0 } 30
