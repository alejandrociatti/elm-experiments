module WFC.Canvas exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Texture as Canvas
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import List.Extra exposing (init)
import Palette.Color exposing (..)
import Palette.Navbar exposing (navbar)
import Palette.Spacing exposing (..)
import Time exposing (posixToMillis)
import WFC.Canvas.Grid exposing (collapse)
import WFC.Canvas.Tileset1 exposing (tiles)
import WFC.Canvas.Types exposing (..)
import WFC.Canvas.View exposing (board)


initSettings : Settings
initSettings =
    { width = 600
    , height = 600
    , dimensions = 10
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        settings =
            initSettings

        textures =
            case List.filterMap Canvas.fromDomImage flags of
                blankTexture :: tail ->
                    List.head tail
                        |> Maybe.map
                            (\downTexture ->
                                { blank = blankTexture, down = downTexture }
                            )

                _ ->
                    Nothing
    in
    ( { time = 0
      , change = False
      , settings = settings
      , grid = initGrid settings
      , lastComputation = 0
      , textures = textures
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame


initGrid : Settings -> List GridTile
initGrid { dimensions } =
    List.range 0 (dimensions * dimensions - 1)
        |> List.map (always (Open tiles))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dimensions =
            model.settings.dimensions

        { lastComputation, grid } =
            model
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AnimationFrame posixTime ->
            let
                time =
                    (toFloat <| posixToMillis posixTime) / 1000

                ( computationTime, newGrid ) =
                    collapseEvery 0.25 lastComputation time dimensions grid
            in
            ( { model
                | time = time
                , lastComputation = computationTime
                , grid = newGrid
              }
            , Cmd.none
            )

        Reset ->
            ( { model
                | grid = initGrid model.settings
              }
            , Cmd.none
            )

        DimensionMinus ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings
                        | dimensions = model.settings.dimensions - 1
                    }
            in
            ( { model
                | settings = newSettings
                , grid = initGrid newSettings
              }
            , Cmd.none
            )

        DimensionPlus ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings
                        | dimensions = model.settings.dimensions + 1
                    }
            in
            ( { model
                | settings = newSettings
                , grid = initGrid newSettings
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill ]
    <|
        column
            [ width fill ]
            [ navbar
            , body [ canvas model ]
            ]


body : List (Element Msg) -> Element Msg
body elements =
    column
        [ width fill, paddingXY s6 s4, spacing s5, Background.color gray35 ]
        elements


canvas : Model -> Element Msg
canvas { settings, grid, textures } =
    let
        canvasSize =
            Tuple.mapBoth round round ( settings.width, settings.height )
    in
    el [ centerX, onLeft leftExplanationText, onRight <| rightCommands settings.dimensions ] <|
        html <|
            Canvas.toHtml canvasSize [] <|
                case textures of
                    Just textures_ ->
                        [ board textures_ settings grid ]

                    Nothing ->
                        []


leftExplanationText : Element Msg
leftExplanationText =
    column
        [ width (fill |> minimum s16 |> maximum 300), padding s2, spacing s2 ]
        [ paragraph
            [ Font.color white
            , Font.size s2
            ]
            [ text "Every 0.25 seconds this tiles collapse to try and coexist" ]
        , image [ centerX ] { src = "assets/blank.png", description = "blank" }
        , image [ centerX ] { src = "assets/down.png", description = "down" }
        ]


rightCommands : Int -> Element Msg
rightCommands dimensions =
    column
        [ width (fill |> maximum 600), padding s2, spacing s2 ]
        [ text "dimensions:"
        , row [ width fill, spacing s2 ]
            [ minusButton dimensions
            , text <| String.fromInt <| dimensions
            , plusButton dimensions
            ]
        , button buttonAttrs { onPress = Just Reset, label = el [ centerX ] <| text "Reset" }
        , link []
            { url = "/graphicsvg"
            , label = text "graphicsvg version"
            }
        ]


minusButton : Int -> Element Msg
minusButton dimensions =
    let
        onPress =
            if dimensions > 3 then
                Just DimensionMinus

            else
                Nothing
    in
    button
        buttonAttrs
        { onPress = onPress
        , label = el [ centerX ] <| text "-"
        }


plusButton : Int -> Element Msg
plusButton dimensions =
    let
        onPress =
            if dimensions < 99 then
                Just DimensionPlus

            else
                Nothing
    in
    button
        buttonAttrs
        { onPress = onPress
        , label = el [ centerX ] <| text "+"
        }


buttonAttrs : List (Attribute msg)
buttonAttrs =
    [ width fill
    , Font.color offwhite
    , Border.color offwhite
    , Background.color gray24
    , Border.solid
    , Border.width 1
    , rounded s2
    , focused []
    , padding s1
    ]


collapseEvery : Float -> Float -> Float -> Int -> List GridTile -> ( Float, List GridTile )
collapseEvery waitSeconds previousTime currentTime dimensions grid =
    if currentTime - previousTime > waitSeconds then
        ( currentTime, collapse dimensions currentTime grid )

    else
        ( previousTime, grid )



-- timeExamples : Model -> List (Shape Msg)
-- timeExamples model =
--     [ circle (25 * sin (3 * model.time) + 25)
--             |> filled
--                 (if model.change then
--                     green
--                  else
--                     orange
--                 )
--     , square 20
--         |> outlined (dashed 2) hotPink
--         |> rotate (degrees (10 * model.time))
--     ]
