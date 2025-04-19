port module Main exposing (..)

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



-- Outgoing port to JavaScript


port downloadSvg : String -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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


init : ( Model, Cmd msg )
init =
    ( { selectedTiling = DeltoidalTriHexagonal
      , selectedTheme = Aqua
      , customStroke = "#000000"
      , customPrimary = "#FFFFFF"
      , customSecondary = "#FFFFFF"
      , customTernary = "#FFFFFF"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectTiling Tiling
    | SelectTheme ColorTheme
    | PickStroke String
    | PickPrimary String
    | PickSecondary String
    | PickTernary String
    | DownloadSvg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTiling tiling ->
            ( { model | selectedTiling = tiling }, Cmd.none )

        SelectTheme theme ->
            ( { model | selectedTheme = theme }, Cmd.none )

        PickStroke color ->
            ( { model | customStroke = color }, Cmd.none )

        PickPrimary color ->
            ( { model | customPrimary = color }, Cmd.none )

        PickSecondary color ->
            ( { model | customSecondary = color }, Cmd.none )

        PickTernary color ->
            ( { model | customTernary = color }, Cmd.none )

        DownloadSvg ->
            ( model, downloadSvg (tessellationName model.selectedTiling) )



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
                [ onClick DownloadSvg
                , class "icon"
                ]
                [ img
                    [ src "assets/save.svg"
                    , width "50"
                    , height "50"
                    ]
                    []
                ]
            , a
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
                )
            ]
        , div [ id "tilingDownload" ]
            [ svg
                [ viewBox "0 0 800 800"
                , width "2400"
                , height "2400"
                ]
                (getTessellation model.selectedTiling
                    (getTheme
                        model.selectedTheme
                        model
                    )
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


getTessellation : Tiling -> (Theme -> List (Svg msg))
getTessellation tiling theme =
    case tiling of
        Square ->
            squareTiling theme 26 26 { x = -5, y = -5 }

        Triangular ->
            triangularTiling theme 20 50 { x = -40, y = -10 }

        Hexagonal ->
            hexagonalTiling theme 16 20 { x = -40, y = -20 }

        TruncatedHexagonal ->
            truncatedHexagonalTiling theme 15 15 { x = 0, y = -15 }

        TriHexagonal ->
            triHexagonalTiling theme 15 17 { x = -5, y = -15 }

        TruncatedSquare ->
            truncatedSquareTiling theme 20 18 { x = -20, y = -10 }

        RhombiTriHexagonal ->
            rhombiTriHexagonalTiling theme 8 25 { x = -10, y = -55 }

        TruncatedTriHexagonal ->
            truncatedTriHexagonalTiling theme 8 20 { x = 3, y = -67 }

        SnubSquare ->
            snubSquareTiling theme 11 22 { x = 0, y = 0 }

        SnubTriHexagonal ->
            snubTriHexagonalTiling theme 5 35 { x = 50, y = -65 }

        ElongatedTriangular ->
            elongatedTriangular theme 30 30 { x = -18, y = 0 }

        TriakisTriangular ->
            triakisTriangularTiling theme 18 18 { x = -55, y = 0 }

        Rhombile ->
            rhombileTiling theme 18 19 { x = -55, y = -10 }

        TetrakisSquare ->
            tetrakisSquareTiling theme 20 20 { x = 0, y = 0 }

        DisdyakisRhombile ->
            disdyakisRhombileTiling theme 15 15 { x = -16, y = 0 }

        DeltoidalTriHexagonal ->
            deltoidalTriHexagonalTiling theme 18 20 { x = 0, y = 0 }

        FloretPentagonal ->
            floretPentagonalTiling theme 18 16 { x = -340, y = -200 }

        CairoPentagonal ->
            cairoTiling theme 15 25 { x = -40, y = 0 }

        PrismaticPentagonal ->
            prismaticPentagonalTiling theme 30 15 { x = -30, y = -10 }

        FloretHexagonal ->
            floretHexaTiling theme 20 25 { x = -20, y = -10 }

        Pythagorean ->
            pythagoreanTiling theme 50 50 { x = -915, y = -813 }

        ConvexHexagonal ->
            convexHexaTiling theme 16 15 { x = -505, y = -205 }


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
            "Regular.elm#L47"

        Hexagonal ->
            "Regular.elm#L86"

        TruncatedHexagonal ->
            "Semiregular.elm#L18"

        TriHexagonal ->
            "Semiregular.elm#L85"

        TruncatedSquare ->
            "Semiregular.elm#L152"

        RhombiTriHexagonal ->
            "Semiregular.elm#L211"

        TruncatedTriHexagonal ->
            "Semiregular.elm#L293"

        SnubSquare ->
            "Semiregular.elm#L375"

        SnubTriHexagonal ->
            "Semiregular.elm#L444"

        ElongatedTriangular ->
            "Semiregular.elm#L521"

        TriakisTriangular ->
            "Laves.elm#L18"

        Rhombile ->
            "Laves.elm#L95"

        TetrakisSquare ->
            "Laves.elm#L157"

        DisdyakisRhombile ->
            "Laves.elm#L203"

        DeltoidalTriHexagonal ->
            "Laves.elm#L268"

        FloretPentagonal ->
            "Laves.elm#L327"

        CairoPentagonal ->
            "Laves.elm#L394"

        PrismaticPentagonal ->
            "Laves.elm#L475"

        FloretHexagonal ->
            "Lab.elm#L14"

        Pythagorean ->
            "Lab.elm#L78"

        ConvexHexagonal ->
            "Lab.elm#L119"


tessellationName : Tiling -> String
tessellationName tiling =
    case tiling of
        Square ->
            "square"

        Triangular ->
            "triangular"

        Hexagonal ->
            "hexagonal"

        TruncatedHexagonal ->
            "truncatedHexagonal"

        TriHexagonal ->
            "trihexagonal"

        TruncatedSquare ->
            "truncatedSquare"

        RhombiTriHexagonal ->
            "rhombiTrihexagonal"

        TruncatedTriHexagonal ->
            "truncatedTrihexagonal"

        SnubSquare ->
            "snubSquare"

        SnubTriHexagonal ->
            "snubTrihexagonal"

        ElongatedTriangular ->
            "elongatedTriangular"

        TriakisTriangular ->
            "triakisTriangular"

        Rhombile ->
            "rhombile"

        TetrakisSquare ->
            "tetrakisSquare"

        DisdyakisRhombile ->
            "disdyakisRhombile"

        DeltoidalTriHexagonal ->
            "deltoidalTrihexagonal"

        FloretPentagonal ->
            "floretPentagonal"

        CairoPentagonal ->
            "cairoPentagonal"

        PrismaticPentagonal ->
            "prismaticPentagonal"

        FloretHexagonal ->
            "floretHexagonal"

        Pythagorean ->
            "pythagorean"

        ConvexHexagonal ->
            "convexHexagonal"
