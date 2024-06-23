module Main exposing(main)

import Types exposing (..)
import View exposing (board)
import GraphicSVG exposing (collage, text, centered, size, filled, move, hotPink)
import GraphicSVG.EllieApp exposing (KeyState(..), Keys(..), gameApp, GameApp)
import GraphicSVG exposing (Collage)
import Tileset1 exposing (tiles)



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

initSettings : Settings
initSettings =
    { width = 600
    , height = 600
    , dimensions = 3
    }

initGrid : Settings -> List GridTile
initGrid { dimensions } =
    List.range 0 (dimensions * dimensions - 1)
        |> List.map(always (Open tiles))


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Tick time ( keyf, ( x0, y0 ), ( x1, y1 ) ) ->
            let
                (computationTime, newGrid) =
                    collapseEvery 5 model.lastComputation time model.grid
            in
            { model 
                | time = time
                , change = keyf (Key "r") == Down
                , lastComputation = computationTime
                , grid = newGrid
            }


view : Model -> Collage Msg
view model =
    collage model.settings.width (model.settings.height + 24)
        [ text "Press \"r\" to reset drawing"
            |> size 24
            |> centered
            |> filled hotPink 
            |> move ( 0, 284 )
        , board model.settings model.grid
        ]


-- collapseEvery 5 time model.grid
collapseEvery : Float -> Float -> Float -> List GridTile -> (Float, List GridTile) 
collapseEvery waitSeconds previousTime currentTime grid =
    if currentTime - previousTime > waitSeconds then
        (currentTime, collapse currentTime grid)
    else
        (previousTime, grid)


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


