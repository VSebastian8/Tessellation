module Shapes exposing (..)

import Polygon exposing (..)
import Util exposing (tripleOp, zip3)


equilateral : Polygon
equilateral =
    { lengths = [ 1, 1, 1 ]
    , angles = [ 60, 60, 60 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


square : Polygon
square =
    { lengths = [ 1, 1, 1, 1 ]
    , angles = [ 90, 90, 90, 90 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


hexagon : Polygon
hexagon =
    { lengths = [ 1, 1, 1, 1, 1, 1 ]
    , angles = [ 120, 120, 120, 120, 120, 120 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


octagon : Polygon
octagon =
    { lengths = [ 1, 1, 1, 1, 1, 1, 1, 1 ]
    , angles = [ 135, 135, 135, 135, 135, 135, 135, 135 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


dodecagon : Polygon
dodecagon =
    { lengths = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
    , angles = [ 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150 ]
    , rotation = 30
    , origin = { x = 0, y = 0 }
    }


obtuseIso : Polygon
obtuseIso =
    { lengths = [ 1, 2 * cos (degrees 30), 1 ]
    , angles = [ 30, 30, 120 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


rhombus : Polygon
rhombus =
    { lengths = [ 1, 1, 1, 1 ]
    , angles = [ 60, 120, 60, 120 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


isosceles : Polygon
isosceles =
    { lengths = [ 1, 2 * cos (degrees 45), 1 ]
    , angles = [ 45, 45, 90 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


right : Polygon
right =
    { lengths = [ 1, cos (degrees 30), cos (degrees 60) ]
    , angles = [ 30, 90, 60 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


left : Polygon
left =
    { lengths = [ 1, cos (degrees 60), cos (degrees 30) ]
    , angles = [ 60, 90, 30 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


kite : Polygon
kite =
    { lengths = [ cos (degrees 30), cos (degrees 60), cos (degrees 60), cos (degrees 30) ]
    , angles = [ 90, 120, 90, 60 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


floret : Polygon
floret =
    { lengths = [ 1 + 2 * cos (degrees 60), 1, 1, 1, 1 + 2 * cos (degrees 60) ]
    , angles = [ 120, 120, 120, 120, 60 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


cairo : Polygon
cairo =
    { lengths = [ sqrt 2 * 2 * cos (degrees 75), 1, 1, 1, 1 ]
    , angles = [ 120, 90, 120, 90, 120 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


prism : Polygon
prism =
    { lengths = [ 1, 1, 1 / (2 * cos (degrees 30)), 1 / (2 * cos (degrees 30)), 1 ]
    , angles = [ 90, 120, 120, 120, 90 ]
    , rotation = 0
    , origin = { x = 0, y = 0 }
    }


{-| (x, y, z) need to form a correct triangle:

Given x, y we have: sqrt(x ^ 2 + y ^ 2 - 1/2) < z < sqrt(x ^ 2 + y ^ 2 + 1/2)

-}
convexHexa : Polygon
convexHexa =
    let
        ( x, y, z ) =
            ( 2.2, 2.5, 1 )

        ( a, b, c ) =
            ( x, y, z ) |> tripleOp (\w -> 2 * w * cos (degrees 30))

        _ =
            Debug.log "Lengths" ( a, b, c )

        ( a_ang, b_ang, c_ang ) =
            zip3
                (( a ^ 2, b ^ 2, c ^ 2 ) |> tripleOp (\s -> a ^ 2 + b ^ 2 + c ^ 2 - 2 * s))
                (( a, b, c ) |> tripleOp (\w -> 2 * a * b * c / w))
                |> tripleOp (\( s, w ) -> s / w)

        _ =
            Debug.log "Small Angles" ( a_ang, b_ang, c_ang )

        ( a_angle, b_angle, c_angle ) =
            ( c_ang, a_ang, b_ang )
                |> tripleOp acos
                |> tripleOp (\w -> w * 57.29578)
                |> tripleOp (\w -> w + 60)

        _ =
            Debug.log "Final Angles" ( a_angle, b_angle, c_angle )
    in
    { lengths = [ x, y, y, z, z, x ]
    , angles = [ a_angle, 120, b_angle, 120, c_angle, 120 ]
    , rotation = 20
    , origin = { x = 0, y = 0 }
    }
