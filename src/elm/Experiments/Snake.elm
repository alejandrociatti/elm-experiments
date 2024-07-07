module Experiments.Snake exposing (Model, Module, Msg, init)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Element exposing (Element)
import Experiments.Snake.Player as Player exposing (Snake)
import Task exposing (Task)
import Utils.Canvas as CU
import Utils.Keyboard as Keyboard exposing (GetKeyState)
import Utils.Layout as Layout
import Utils.Random as Random
import Utils.TickWithKeys as TickWithKeys
import Utils.Vector2 as V



-- MODULE PATTERN


type alias Module msg model =
    { view : model -> Element msg
    , update : Msg -> model -> ( model, Cmd msg )
    , init : ( Model, Cmd msg )
    , subscriptions : model -> Sub msg
    }


init :
    { toModel : model -> Model -> model
    , fromModel : model -> Maybe Model
    , toMsg : Msg -> msg
    }
    -> Module msg model
init { toModel, fromModel, toMsg } =
    let
        mapView : model -> Element msg
        mapView model =
            fromModel model
                |> Maybe.map (Element.map toMsg << view)
                |> Maybe.withDefault Element.none

        unmapUpdate : model -> ( Model, Cmd Msg ) -> ( model, Cmd msg )
        unmapUpdate userModel ( innerModel, innerCmd ) =
            ( toModel userModel innerModel
            , Cmd.map toMsg innerCmd
            )

        mapUpdate_ : Msg -> model -> Model -> ( model, Cmd msg )
        mapUpdate_ msg userModel innerModel =
            update msg innerModel
                |> unmapUpdate userModel

        mapUpdate : Msg -> model -> ( model, Cmd msg )
        mapUpdate msg model =
            case fromModel model of
                Just model_ ->
                    mapUpdate_ msg model model_

                Nothing ->
                    ( model, Cmd.none )

        mapSubscriptions : model -> Sub msg
        mapSubscriptions model =
            fromModel model
                |> Maybe.map (\model_ -> subscriptions model_ |> Sub.map toMsg)
                |> Maybe.withDefault Sub.none
    in
    { view = mapView
    , update = mapUpdate
    , init =
        Tuple.mapSecond (Cmd.map toMsg) init_
    , subscriptions = mapSubscriptions
    }



-- TYPES


type alias Model =
    { snake : Snake
    , food : ( Int, Int )
    , score : Int
    , tickAndKeys : TickWithKeys.Model
    , frame : Int
    , skipFrames : Int
    }


type Msg
    = NoOp
    | Tick Float GetKeyState
    | GotTKMsg TickWithKeys.Msg



-- INIT, VIEW, UPDATE, SUBSCRIPTIONS


init_ : ( Model, Cmd Msg )
init_ =
    ( { snake = Player.init
      , food = ( 0, 0 )
      , score = 0
      , tickAndKeys = TickWithKeys.init
      , frame = 0
      , skipFrames = 20
      }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    Layout.view
        { width = 600
        , height = 600
        , onLeft = Nothing
        , onRight = Nothing
        }
        (game model)


game : Model -> List Canvas.Renderable
game { snake, food, score } =
    let
        snakeRenderable =
            Player.render snake

        foodRenderable =
            Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect (V.fromInt food) 1 1 ]

        scoreRenderable =
            Canvas.text [] ( 0, 0 ) (String.fromInt score)
    in
    [ CU.scale 20 20 <|
        Canvas.group [] [ snakeRenderable, foodRenderable ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTKMsg tkMsg ->
            let
                newTK =
                    TickWithKeys.update tkMsg model.tickAndKeys

                newFrame =
                    model.frame + 1

                cmd =
                    if modBy (model.skipFrames + 1) newFrame == 0 then
                        Task.perform
                            (always <| TickWithKeys.toTickMsg newTK Tick)
                            (Task.succeed ())

                    else
                        Cmd.none
            in
            ( { model | tickAndKeys = newTK, frame = newFrame }
            , cmd
            )

        Tick time ( keyf, arrows, wasd ) ->
            let
                ( eat, newSnake ) =
                    Player.eat
                        (Player.update (V.toInt wasd) model.snake)
                        model.food

                newFood =
                    if eat then
                        ( Random.randomBetween 0 29 time, Random.randomBetween 0 29 (time + 0.001) )

                    else
                        model.food
            in
            ( { model | snake = newSnake, food = newFood }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    TickWithKeys.subscriptions
        |> List.map (Sub.map GotTKMsg)
        |> Sub.batch
