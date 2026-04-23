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
            , size = 30
            }

        squareRule2 =
            { anchor = { squ | col = Ternary }
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Secondary }
                , tr { x = 2, y = 0 } { squ | col = Ternary }
                ]
            , rotatable = False
            , size = 30
            }

        squareRule3 =
            { anchor = squ
            , additions = [ tr { x = -0.5, y = 1 } { squ | col = Ternary } ]
            , rotatable = False
            , size = 30
            }

        squareRule4 =
            { anchor = { squ | col = Ternary }
            , additions = [ tr { x = 0.5, y = 1 } squ ]
            , rotatable = False
            , size = 30
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
    , size = 30
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
            , size = 30
            }

        triRule2 =
            { anchor = eqi
            , additions =
                [ eqi |> tr (equilateral |> getPoint 2) |> tr { x = -0.5, y = 0 } ]
            , rotatable = False
            , size = 30
            }
    in
    { rules = [ triRule1, triRule2 ]
    , open = [ eqi |> tr { x = -0.5, y = 0 } ]
    , closed = []
    , bounds = ( { x = -1, y = -1 }, { x = 28, y = 28 } )
    , size = 30
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
            , size = 30
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
            , size = 30
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
            , size = 30
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
    , size = 10
    }


trithagoreanTessellation : Tess
trithagoreanTessellation =
    let
        triRule1 =
            { anchor = eqi |> sz 3
            , additions =
                [ { eqi | col = Ternary } |> rt { x = 0, y = 0 } 60 |> tr { x = 2, y = 0 }
                , { eqi | col = Ternary }
                    |> rt { x = 0, y = 0 } 60
                    |> tr (equilateral |> getPoint 2 |> mul 3)
                , { eqi | col = Ternary }
                    |> rt { x = 0, y = 0 } 60
                    |> tr (equilateral |> setRotation 60 |> getPoint 1 |> neg)
                ]
            , rotatable = False
            , size = 30
            }

        triRule2 =
            { anchor = { eqi | col = Ternary } |> rt { x = 0, y = 0 } 60 |> tr { x = 2, y = 5 }
            , additions =
                [ { eqi | col = Secondary } |> sz 2 |> rt { x = 0, y = 0 } 60 |> tr { x = 0, y = 5 }
                , { eqi | col = Secondary } |> sz 2 |> rt { x = 0, y = 0 } 60 |> tr { x = 2, y = 5 } |> tr (equilateral |> setRotation 60 |> getPoint 1)
                , { eqi | col = Secondary } |> sz 2 |> rt { x = 0, y = 0 } 60 |> tr { x = 2, y = 5 } |> tr (equilateral |> setRotation 60 |> getPoint 2) |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2 |> neg)
                ]
            , rotatable = False
            , size = 30
            }

        triRule3 =
            { anchor = { eqi | col = Secondary } |> sz 2 |> rt { x = 0, y = 0 } 60 |> tr { x = 2, y = 8 }
            , additions =
                [ eqi |> sz 3 |> tr { x = 2, y = 8 }
                , eqi |> sz 3 |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2) |> tr { x = -1, y = 8 }
                , eqi |> sz 3 |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2) |> tr (equilateral |> getPoint 2 |> neg) |> tr { x = 2, y = 8 }
                ]
            , rotatable = False
            , size = 30
            }
    in
    { rules =
        [ triRule1
        , triRule2
        , triRule3
        ]
    , open = [ eqi |> sz 3 |> tr { x = 25, y = 25 } ]
    , closed = []
    , bounds = ( { x = -2, y = -2 }, { x = 100, y = 100 } )
    , size = 15
    }


hexthagoreanTessellation : Tess
hexthagoreanTessellation =
    let
        hx =
            hexv |> sz 2

        hexRule1 =
            { anchor = hx
            , additions =
                [ { eqi | col = Ternary } |> rt { x = 0, y = 0 } 60
                , { eqi | col = Ternary } |> tr (hx |> pt 1)
                , { eqi | col = Ternary } |> rt { x = 0, y = 0 } -60 |> tr (hx |> pt 2)
                , { eqi | col = Ternary } |> rt { x = 0, y = 0 } -120 |> tr (hx |> pt 3)
                , { eqi | col = Ternary } |> rt { x = 0, y = 0 } -180 |> tr (hx |> pt 4)
                , { eqi | col = Ternary } |> rt { x = 0, y = 0 } 120 |> tr (hx |> pt 5)
                ]
            , rotatable = True
            , size = 30
            }

        eq1 =
            { eqi | col = Ternary } |> rt { x = 0, y = 0 } 180 |> tr { x = 2, y = 5 }

        triRule1 =
            { anchor = eq1
            , additions =
                [ { hx | col = Secondary } |> tr (eq1 |> pt 1)
                ]
            , rotatable = True
            , size = 30
            }

        hexRule2 =
            { anchor = { hx | col = Secondary } |> tr { x = 0, y = 8 }
            , additions =
                [ hx |> tr { x = -1, y = 8 } |> tr (pt 4 hx)
                , { eqi | col = Quart } |> rt { x = 0, y = 0 } -60 |> tr (hx |> pt 2) |> tr { x = 0, y = 8 }
                ]
            , rotatable = True
            , size = 30
            }
    in
    { rules =
        [ hexRule1
        , triRule1
        , hexRule2
        ]
    , open = [ hexv |> rt { x = 0, y = 0 } 0 |> sz 2 |> tr { x = 20, y = 20 } ]
    , closed = []
    , bounds = ( { x = -2, y = -2 }, { x = 100, y = 100 } )
    , size = 20
    }
