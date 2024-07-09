module Experiments.WFC.Svg exposing (main)

import GraphicSVG exposing (Collage, centered, collage, filled, hotPink, move, size, text)
import GraphicSVG.EllieApp exposing (GameApp, KeyState(..), Keys(..), gameApp)
import List.Extra exposing (init)
import Experiments.WFC.Svg.Grid exposing (collapse)
import Experiments.WFC.Svg.Tileset1 exposing (tiles)
import Experiments.WFC.Svg.Types exposing (..)
import Experiments.WFC.Svg.View exposing (board)


initSettings : Settings
initSettings =
    { width = 600
    , height = 600
    , dimensions = 10
    }


main : GameApp Model Msg
main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        , title = "GameApp Template"
        }


init : Model
init =
    let
        settings =
            initSettings
    in
    { time = 0
    , change = False
    , settings = settings
    , grid = initGrid settings
    , lastComputation = 0
    }


initGrid : Settings -> List GridTile
initGrid { dimensions } =
    List.range 0 (dimensions * dimensions - 1)
        |> List.map (always (Open tiles))


update : Msg -> Model -> Model
update msg model =
    let
        dimensions =
            model.settings.dimensions

        { lastComputation, grid } =
            model
    in
    case msg of
        NoOp ->
            model

        Tick time ( keyf, ( x0, y0 ), ( x1, y1 ) ) ->
            if keyf (Key "r") == Down then
                init

            else
                let
                    ( computationTime, newGrid ) =
                        collapseEvery 0.25 lastComputation time dimensions grid
                in
                { model
                    | time = time
                    , lastComputation = computationTime
                    , grid = newGrid
                }


view : Model -> Collage Msg
view model =
    collage model.settings.width
        (model.settings.height + 24)
        [ text "Press \"r\" to reset drawing"
            |> size 24
            |> centered
            |> filled hotPink
            |> move ( 0, 284 )
        , board model.settings model.grid
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
