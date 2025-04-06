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


flowerPentagon : Polygon
flowerPentagon =
    { lengths = [ 1, 1, 1, 2, 2 ], angles = [ 120, 120, 120, 120, 60 ], rotation = 60 }
