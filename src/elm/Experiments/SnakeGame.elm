module Experiments.SnakeGame exposing (Model, Module, Msg, init)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Element exposing (..)
import Element.Font as Font exposing (Font)
import Experiments.Snake.Snake as Player exposing (Snake)
import Palette.Color exposing (..)
import Palette.Spacing exposing (s2)
import Task exposing (Task)
import Utils.Canvas as CU
import Utils.Keyboard as Keyboard exposing (GetKeyState)
import Utils.Layout as Layout
import Utils.Number exposing (constrain)
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
    | Tick TickWithKeys.Msg



-- INIT, VIEW, UPDATE, SUBSCRIPTIONS


init_ : ( Model, Cmd Msg )
init_ =
    let
        snake =
            Player.init
    in
    ( { snake = snake
      , food = newFood 0 snake.body
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
        , onLeft = Just <| leftExplanation
        , onRight = Just <| score model.score
        }
        (game model)


leftExplanation : Element Msg
leftExplanation =
    column
        [ width <| px 300, padding s2 ]
        [ paragraph
            [ Font.color white ]
            [ text "Simple snake game, press r to reset after you die, "
            , text "move with WASD"
            ]
        ]


score : Int -> Element Msg
score s =
    column
        [ width <| px 300, padding s2 ]
        [ paragraph
            [ Font.color white ]
            [ text "Score: "
            , text <| String.fromInt s
            ]
        ]


game : Model -> List Canvas.Renderable
game { snake, food } =
    let
        snakeRenderable =
            Player.render snake

        foodRenderable =
            Canvas.shapes [ Canvas.fill (CU.color green) ] [ Canvas.rect (V.fromInt food) 1 1 ]
    in
    [ CU.scale 20 20 <|
        Canvas.group [] [ snakeRenderable, foodRenderable ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick tkMsg ->
            let
                newTK =
                    TickWithKeys.update tkMsg model.tickAndKeys

                newModel =
                    { model | tickAndKeys = newTK }
            in
            case tkMsg of
                TickWithKeys.TickMsg _ ->
                    logicUpdate newTK.time.elapsed newTK.state newModel

                _ ->
                    ( newModel, Cmd.none )


logicUpdate : Float -> GetKeyState -> Model -> ( Model, Cmd Msg )
logicUpdate time (( keyf, _, wasd ) as gks) model =
    if keyf (Keyboard.Key "r") == Keyboard.Down && not model.snake.living then
        init_

    else
        let
            newSnake =
                Player.logicUpdate wasd model.snake

            newFrame =
                model.frame + 1

            newModel =
                { model | snake = newSnake, frame = newFrame }
        in
        if modBy (model.skipFrames + 1) newFrame == 0 then
            gameUpdate time gks newModel

        else
            ( newModel, Cmd.none )


gameUpdate : Float -> GetKeyState -> Model -> ( Model, Cmd Msg )
gameUpdate time ( keyf, arrows, wasd ) model =
    let
        ( eat, newSnake ) =
            Player.eat
                (Player.update model.snake)
                model.food

        ( newFood_, newScore ) =
            if eat then
                ( newFood time newSnake.body, model.score + 5 )

            else
                ( model.food, model.score )

        skipFrames =
            if eat && modBy 3 model.snake.size == 0 then
                constrain 10 99 (model.skipFrames - 1)

            else
                model.skipFrames
    in
    ( { model | snake = newSnake, food = newFood_, skipFrames = skipFrames, score = newScore }, Cmd.none )


newFood : Float -> List ( Int, Int ) -> ( Int, Int )
newFood time body =
    let
        x =
            Random.randomBetween 0 29 time

        y =
            Random.randomBetween 0 29 (time + 0.001)
    in
    if List.member ( x, y ) body then
        newFood (time + 0.002) body

    else
        ( x, y )


subscriptions : Model -> Sub Msg
subscriptions model =
    TickWithKeys.subscriptions
        |> List.map (Sub.map Tick)
        |> Sub.batch
