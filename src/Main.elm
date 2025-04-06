module Main exposing (main)

import Browser
import ColorTheme exposing (Color(..), Theme, amethystTheme, aquaTheme, honeyTheme)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, href, name, style, target, type_)
import Html.Events exposing (onInput)
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


type ColorTheme
    = Amethyst
    | Aqua
    | Honey


init : Model
init =
    { selectedTiling = RhombiTriHexagonal
    , selectedTheme = Aqua
    }



-- UPDATE


type Msg
    = SelectTiling Tiling
    | SelectTheme ColorTheme


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectTiling tiling ->
            { model | selectedTiling = tiling }

        SelectTheme theme ->
            { model | selectedTheme = theme }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "300px 1fr 300px"
        , style "grid-template-rows" "auto auto 1fr"
        , style "height" "100vh"
        , style "padding" "20px"
        , style "gap" "20px"
        , style "box-sizing" "border-box"
        ]
        [ -- Left Top Panel
          div
            [ style "grid-column" "1"
            , style "grid-row" "1"
            , style "width" "250px"
            , style "height" "auto"
            , style "padding" "20px"
            , style "background" "white"
            , style "border" "1px solid #e0e0e0"
            , style "border-radius" "8px"
            , style "margin-top" "50px"
            , style "box-shadow" "0 2px 8px rgba(0,0,0,0.1)"
            , attribute "data-test-id" "left-top-panel"
            ]
            [ h2 [] [ text "Tiling:" ]
            , div [ style "margin-bottom" "15px" ]
                [ tilingRadio Square "Square" model
                , tilingRadio Triangular "Triangular" model
                , tilingRadio TruncatedHexagonal "Truncated Hexagonal" model
                , tilingRadio TriHexagonal "Trihexagonal" model
                , tilingRadio TruncatedSquare "Truncated Square" model
                , tilingRadio RhombiTriHexagonal "Rhombitrihexagonal" model
                , tilingRadio TruncatedTriHexagonal "Truncated Trihexagonal" model
                , tilingRadio SnubSquare "Snub Square" model
                , tilingRadio SnubTriHexagonal "Snub Trihexagonal" model
                , tilingRadio ElongatedTriangular "Elongated Triangular" model
                ]
            ]
        , -- Left Middle Panel
          div
            [ style "grid-column" "1"
            , style "grid-row" "2"
            , style "width" "250px"
            , style "height" "auto"
            , style "padding" "20px"
            , style "background" "white"
            , style "border" "1px solid #e0e0e0"
            , style "border-radius" "8px"
            , style "box-shadow" "0 2px 8px rgba(0,0,0,0.1)"
            , attribute "data-test-id" "left-bottom-panel"
            ]
            [ h2 [] [ text "Theme:" ]
            , div [ style "margin-bottom" "15px" ]
                [ themeRadio Amethyst "Amethyst" model
                , themeRadio Aqua "Aqua" model
                , themeRadio Honey "Honey" model
                ]
            ]
        , -- Left Bottom Panel
          div
            [ style "grid-column" "1"
            , style "grid-row" "3"
            , style "width" "250px"
            , style "height" "100px"
            , style "padding" "20px"
            , style "background" "white"
            , style "border" "1px solid #e0e0e0"
            , style "border-radius" "8px"
            , style "box-shadow" "0 2px 8px rgba(0,0,0,0.1)"
            , attribute "data-test-id" "left-bottom-panel"
            ]
            [ h2 [] [ text "Github:" ]
            , a
                [ href "https://github.com/VSebastian8/Tessellation"
                , target "_blank" -- Opens in new tab
                , style "color" "#3a7bd5"
                , style "text-decoration" "underline"
                ]
                [ text "VSebastian8/Tessellation" ]
            ]
        , -- Center Content
          div
            [ style "grid-column" "2"
            , style "grid-row" "1 / 4" -- Span all rows
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , attribute "data-test-id" "center-content"
            ]
            [ svg
                [ viewBox "0 0 800 800"
                , width "800"
                , height "800"
                ]
                (getTessellation model.selectedTiling
                    (getTheme
                        model.selectedTheme
                    )
                    50
                    50
                    { x = -50, y = -50 }
                )
            ]
        ]


tilingRadio : Tiling -> String -> Model -> Html Msg
tilingRadio tilingValue labelText model =
    label
        [ style "display" "block"
        , style "margin" "10px 0"
        ]
        [ input
            [ type_ "radio"
            , name "tiling-selection"
            , checked (model.selectedTiling == tilingValue)
            , onInput (\_ -> SelectTiling tilingValue)
            , style "margin-right" "8px"
            ]
            []
        , text labelText
        ]


themeRadio : ColorTheme -> String -> Model -> Html Msg
themeRadio themeValue labelText model =
    label
        [ style "display" "block"
        , style "margin" "10px 0"
        ]
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


getTheme : ColorTheme -> Theme
getTheme theme =
    case theme of
        Amethyst ->
            amethystTheme

        Aqua ->
            aquaTheme

        Honey ->
            honeyTheme
