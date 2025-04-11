module Main exposing (main)

import Browser
import ColorTheme exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, name, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Lab exposing (..)
import Laves exposing (..)
import Regular exposing (..)
import Semiregular exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import Util exposing (Point)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { selectedTiling : Tiling
    , selectedTheme : ColorTheme
    , customStroke : String
    , customPrimary : String
    , customSecondary : String
    , customTernary : String
    }


type Tiling
    = Square
    | Triangular
    | Hexagonal
    | TruncatedHexagonal
    | TriHexagonal
    | TruncatedSquare
    | RhombiTriHexagonal
    | TruncatedTriHexagonal
    | SnubSquare
    | SnubTriHexagonal
    | ElongatedTriangular
    | TriakisTriangular
    | Rhombile
    | TetrakisSquare
    | DisdyakisRhombile
    | DeltoidalTriHexagonal
    | FloretPentagonal
    | CairoPentagonal
    | PrismaticPentagonal
    | FloretHexagonal
    | Pythagorean
    | ConvexHexagonal


type ColorTheme
    = Amethyst
    | Aqua
    | Honey
    | Forest
    | Custom


init : Model
init =
    { selectedTiling = DeltoidalTriHexagonal
    , selectedTheme = Aqua
    , customStroke = "#000000"
    , customPrimary = "#FFFFFF"
    , customSecondary = "#FFFFFF"
    , customTernary = "#FFFFFF"
    }



-- UPDATE


type Msg
    = SelectTiling Tiling
    | SelectTheme ColorTheme
    | PickStroke String
    | PickPrimary String
    | PickSecondary String
    | PickTernary String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectTiling tiling ->
            { model | selectedTiling = tiling }

        SelectTheme theme ->
            { model | selectedTheme = theme }

        PickStroke color ->
            { model | customStroke = color }

        PickPrimary color ->
            { model | customPrimary = color }

        PickSecondary color ->
            { model | customSecondary = color }

        PickTernary color ->
            { model | customTernary = color }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "container" ]
        [ -- Left Top Panel
          div
            [ id "tilingContainer" ]
            [ tilingRadio Square "Square" model
            , tilingRadio TruncatedHexagonal "Truncated Hexagonal" model
            , tilingRadio Triangular "Triangular" model
            , tilingRadio TruncatedSquare "Truncated Square" model
            , tilingRadio Hexagonal "Hexagonal" model
            , tilingRadio RhombiTriHexagonal "Rhombitrihexagonal" model
            , tilingRadio TriHexagonal "Trihexagonal" model
            , tilingRadio TruncatedTriHexagonal "Truncated Trihexagonal" model
            , tilingRadio SnubSquare "Snub Square" model
            , tilingRadio SnubTriHexagonal "Snub Trihexagonal" model
            , tilingRadio Pythagorean "Pythagorean" model
            , tilingRadio ElongatedTriangular "Elongated Triangular" model
            , tilingRadio Rhombile "Rhombile" model
            , tilingRadio TriakisTriangular "Triakis Triangular" model
            , tilingRadio TetrakisSquare "Tetrakis Square" model
            , tilingRadio DisdyakisRhombile "Disdyakis Rhombile" model
            , tilingRadio FloretPentagonal "Floret Pentagonal" model
            , tilingRadio DeltoidalTriHexagonal "Deltoidal Trihexagonal" model
            , tilingRadio CairoPentagonal "Cairo Pentagonal" model
            , tilingRadio PrismaticPentagonal "Prismatic Pentagonal" model
            , tilingRadio FloretHexagonal "Floret Hexagonal" model
            , tilingRadio ConvexHexagonal "Convex Hexagonal" model
            ]
        , -- Left Middle Panel
          div
            [ id "themeContainer" ]
            [ h2 [] [ text "Theme:" ]
            , div []
                [ themeRadio Amethyst "Amethyst" model
                , themeRadio Aqua "Aqua" model
                , themeRadio Honey "Honey" model
                , themeRadio Forest "Forest" model
                , themeRadio Custom "Custom" model
                , colorPicker Primary "Primary" model
                , colorPicker Secondary "Secondary" model
                , colorPicker Ternary "Ternary" model
                , colorPicker Quart "Stroke" model
                ]
            ]
        , -- Left Bottom Panel
          div
            [ id "infoContainer" ]
            [ a
                [ href "https://github.com/VSebastian8/Tessellation"
                , target "_blank" -- Opens in new tab
                , class "icon"
                ]
                [ img
                    [ src "assets/docs.svg"
                    , width "50"
                    , height "50"
                    ]
                    []
                ]
            , a
                [ href ("https://github.com/VSebastian8/Tessellation/tree/main/src/" ++ tessellationLink model.selectedTiling)
                , target "_blank" -- Opens in new tab
                , class "icon"
                ]
                [ img
                    [ src "assets/code.svg"
                    , width "50"
                    , height "50"
                    ]
                    []
                ]
            , a
                [ href "https://github.com/VSebastian8/Tessellation/blob/main/CHANGELOG.md"
                , target "_blank" -- Opens in new tab
                , class "icon"
                ]
                [ img
                    [ src "assets/switch.svg"
                    , width "50"
                    , height "50"
                    ]
                    []
                ]
            ]
        , -- Center Content
          div
            [ id "svgContainer" ]
            [ svg
                [ viewBox "0 0 800 800"
                , width "800"
                , height "800"
                ]
                (getTessellation model.selectedTiling
                    (getTheme
                        model.selectedTheme
                        model
                    )
                    60
                    90
                    { x = -750, y = -680 }
                )
            ]
        ]


tilingRadio : Tiling -> String -> Model -> Html Msg
tilingRadio tilingValue tilingText model =
    button
        [ class "tiling"
        , class
            (if model.selectedTiling == tilingValue then
                "selected"

             else
                ""
            )
        , onClick (SelectTiling tilingValue)
        ]
        [ text tilingText ]


themeRadio : ColorTheme -> String -> Model -> Html Msg
themeRadio themeValue labelText model =
    label
        [ class "theme" ]
        [ input
            [ type_ "radio"
            , name "theme-selection"
            , checked (model.selectedTheme == themeValue)
            , onInput (\_ -> SelectTheme themeValue)
            , style "margin-right" "8px"
            ]
            []
        , text labelText
        ]


colorPicker : Color -> String -> Model -> Html Msg
colorPicker colorType labelText model =
    label
        [ class "customColor" ]
        [ input
            [ type_ "color"
            , name "color-picker"
            , value
                (case colorType of
                    Primary ->
                        model.customPrimary

                    Secondary ->
                        model.customSecondary

                    Ternary ->
                        model.customTernary

                    Quart ->
                        model.customStroke
                )
            , onInput
                (\value ->
                    case colorType of
                        Primary ->
                            PickPrimary value

                        Secondary ->
                            PickSecondary value

                        Ternary ->
                            PickTernary value

                        Quart ->
                            PickStroke value
                )
            ]
            []
        , text labelText
        ]



--   <label for="favcolor">Select your favorite color:</label>
--   <input type="color" id="favcolor" name="favcolor" value="#ff0000"><br><br>


getTessellation : Tiling -> (Theme -> Int -> Int -> Point -> List (Svg msg))
getTessellation tiling =
    case tiling of
        Square ->
            squareTiling

        Triangular ->
            triangularTiling

        Hexagonal ->
            hexagonalTiling

        TruncatedHexagonal ->
            truncatedHexagonalTiling

        TriHexagonal ->
            triHexagonalTiling

        TruncatedSquare ->
            truncatedSquareTiling

        RhombiTriHexagonal ->
            rhombiTriHexagonalTiling

        TruncatedTriHexagonal ->
            truncatedTriHexagonalTiling

        SnubSquare ->
            snubSquareTiling

        SnubTriHexagonal ->
            snubTriHexagonalTiling

        ElongatedTriangular ->
            elongatedTriangular

        TriakisTriangular ->
            triakisTriangularTiling

        Rhombile ->
            rhombileTiling

        TetrakisSquare ->
            tetrakisSquareTiling

        DisdyakisRhombile ->
            disdyakisRhombileTiling

        DeltoidalTriHexagonal ->
            deltoidalTriHexagonalTiling

        FloretPentagonal ->
            floretPentagonalTiling

        CairoPentagonal ->
            cairoTiling

        PrismaticPentagonal ->
            prismaticPentagonalTiling

        FloretHexagonal ->
            floretHexaTiling

        Pythagorean ->
            pythagoreanTiling

        ConvexHexagonal ->
            convexHexaTiling


getTheme : ColorTheme -> Model -> Theme
getTheme theme model =
    case theme of
        Amethyst ->
            amethystTheme

        Aqua ->
            aquaTheme

        Honey ->
            honeyTheme

        Forest ->
            forestTheme

        Custom ->
            { strokeColor = model.customStroke
            , getColor =
                \color ->
                    case color of
                        Primary ->
                            model.customPrimary

                        Secondary ->
                            model.customSecondary

                        Ternary ->
                            model.customTernary

                        Quart ->
                            model.customStroke
            }


tessellationLink : Tiling -> String
tessellationLink tiling =
    case tiling of
        Square ->
            "Regular.elm#L18"

        Triangular ->
            "Regular.elm#L41"

        Hexagonal ->
            "Regular.elm#L80"

        TruncatedHexagonal ->
            "Semiregular.elm#L18"

        TriHexagonal ->
            "Semiregular.elm#L71"

        TruncatedSquare ->
            "Semiregular.elm#L124"

        RhombiTriHexagonal ->
            "Semiregular.elm#L167"

        TruncatedTriHexagonal ->
            "Semiregular.elm#L246"

        SnubSquare ->
            "Semiregular.elm#L325"

        SnubTriHexagonal ->
            "Semiregular.elm#L389"

        ElongatedTriangular ->
            "Semiregular.elm#L459"

        TriakisTriangular ->
            "Laves.elm#L17"

        Rhombile ->
            "Laves.elm#L89"

        TetrakisSquare ->
            "Laves.elm#L148"

        DisdyakisRhombile ->
            "Laves.elm#L192"

        DeltoidalTriHexagonal ->
            "Laves.elm#L251"

        FloretPentagonal ->
            "Laves.elm#L308"

        CairoPentagonal ->
            "Laves.elm#L373"

        PrismaticPentagonal ->
            "Laves.elm#L458"

        FloretHexagonal ->
            "Lab.elm#L13"

        Pythagorean ->
            "Lab.elm#L75"

        ConvexHexagonal ->
            "Lab.elm#L116"
