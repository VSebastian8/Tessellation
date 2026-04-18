module RuleBased.Isogonal exposing (..)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


squareRowsTessellation : Tess
squareRowsTessellation =
    let
        squareRule1 =
            { anchor = squ
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Quart }
                , tr { x = 2, y = 0 } squ
                ]
            , rotatable = False
            }

        squareRule2 =
            { anchor = { squ | col = Ternary }
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Secondary }
                , tr { x = 2, y = 0 } { squ | col = Ternary }
                ]
            , rotatable = False
            }

        squareRule3 =
            { anchor = squ
            , additions = [ tr { x = -0.5, y = 1 } { squ | col = Ternary } ]
            , rotatable = False
            }

        squareRule4 =
            { anchor = { squ | col = Ternary }
            , additions = [ tr { x = 0.5, y = 1 } squ ]
            , rotatable = False
            }
    in
    { rules =
        [ squareRule1
        , squareRule2
        , squareRule3
        , squareRule4
        ]
    , open = [ squ ]
    , closed = []
    , bounds = ( { x = -1, y = -1 }, { x = 28, y = 28 } )
    }


triangularRowsTessellation : Tess
triangularRowsTessellation =
    let
        triRule1 =
            { anchor = eqi
            , additions =
                [ { eqi | col = Secondary } |> rt { x = 0, y = 0 } 60 |> tr (equilateral |> getPoint 2)
                , tr { x = 1, y = 0 } eqi
                ]
            , rotatable = False
            }

        triRule2 =
            { anchor = eqi
            , additions =
                [ eqi |> tr (equilateral |> getPoint 2) |> tr { x = -0.5, y = 0 } ]
            , rotatable = False
            }
    in
    { rules = [ triRule1, triRule2 ]
    , open = [ eqi |> tr { x = -0.5, y = 0 } ]
    , closed = []
    , bounds = ( { x = -1, y = -1 }, { x = 28, y = 28 } )
    }


pythagoreanTessellation : Tess
pythagoreanTessellation =
    let
        squareRule1 =
            { anchor = squ |> sz 3
            , additions =
                [ { squ | col = Ternary } |> tr { x = 3, y = 0 }
                , { squ | col = Ternary } |> tr { x = 2, y = 3 } |> rt { x = 2.5, y = 3.5 } 90
                , { squ | col = Ternary } |> tr { x = -1, y = 2 }
                , { squ | col = Ternary } |> tr { x = 0, y = -1 } |> rt { x = 0.5, y = -0.5 } 90
                ]
            , rotatable = False
            }

        squareRule2 =
            { anchor = { squ | col = Ternary } |> tr { x = 1, y = 5 }
            , additions =
                [ squ |> sz 3 |> tr { x = 2, y = 3 }
                , { squ | col = Secondary } |> sz 3 |> tr { x = -1, y = 2 }
                , squ |> sz 3 |> tr { x = -2, y = 5 }
                , { squ | col = Secondary } |> sz 3 |> tr { x = 1, y = 6 }
                ]
            , rotatable = False
            }

        squareRule3 =
            { anchor = { squ | col = Ternary } |> tr { x = 1, y = 10 } |> rt { x = 1.5, y = 10.5 } 90
            , additions =
                [ { squ | col = Secondary } |> sz 3 |> tr { x = 2, y = 8 }
                , squ |> sz 3 |> tr { x = -1, y = 7 }
                , { squ | col = Secondary } |> sz 3 |> tr { x = -2, y = 10 }
                , squ |> sz 3 |> tr { x = 1, y = 11 }
                ]
            , rotatable = False
            }
    in
    { rules =
        [ squareRule1
        , squareRule2
        , squareRule3
        ]
    , open = [ squ |> sz 3 |> tr { x = 25, y = 25 } ]
    , closed = []
    , bounds = ( { x = -2, y = -2 }, { x = 100, y = 100 } )
    }
