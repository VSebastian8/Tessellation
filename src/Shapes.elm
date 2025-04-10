module Shapes exposing (..)

import Polygon exposing (..)


equilateral : Polygon
equilateral =
    { lengths = [ 1, 1, 1 ]
    , angles = [ 60, 60, 60 ]
    , rotation = 0
    }


square : Polygon
square =
    { lengths = [ 1, 1, 1, 1 ]
    , angles = [ 90, 90, 90, 90 ]
    , rotation = 0
    }


hexagon : Polygon
hexagon =
    { lengths = [ 1, 1, 1, 1, 1, 1 ]
    , angles = [ 120, 120, 120, 120, 120, 120 ]
    , rotation = 0
    }


octagon : Polygon
octagon =
    { lengths = [ 1, 1, 1, 1, 1, 1, 1, 1 ]
    , angles = [ 135, 135, 135, 135, 135, 135, 135, 135 ]
    , rotation = 0
    }


dodecagon : Polygon
dodecagon =
    { lengths = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
    , angles = [ 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150 ]
    , rotation = 30
    }


obtuseIso : Polygon
obtuseIso =
    { lengths = [ 1, 2 * cos (degrees 30), 1 ], angles = [ 30, 30, 120 ], rotation = 0 }


rhombus : Polygon
rhombus =
    { lengths = [ 1, 1, 1, 1 ], angles = [ 60, 120, 60, 120 ], rotation = 0 }


isosceles : Polygon
isosceles =
    { lengths = [ 1, 2 * cos (degrees 45), 1 ], angles = [ 45, 45, 90 ], rotation = 0 }


right : Polygon
right =
    { lengths = [ 1, cos (degrees 30), cos (degrees 60) ]
    , angles = [ 30, 90, 60 ]
    , rotation = 0
    }


left : Polygon
left =
    { lengths = [ 1, cos (degrees 60), cos (degrees 30) ]
    , angles = [ 60, 90, 30 ]
    , rotation = 0
    }


kite : Polygon
kite =
    { lengths = [ cos (degrees 30), cos (degrees 60), cos (degrees 60), cos (degrees 30) ]
    , angles = [ 90, 120, 90, 60 ]
    , rotation = 0
    }


floret : Polygon
floret =
    { lengths = [ 1 + 2 * cos (degrees 60), 1, 1, 1, 1 + 2 * cos (degrees 60) ]
    , angles = [ 120, 120, 120, 120, 60 ]
    , rotation = 0
    }
