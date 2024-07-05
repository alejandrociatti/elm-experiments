module Experiments.Raycast.Raycast03 exposing (Model, Module, Msg, init)

import Browser.Events exposing (onAnimationFrame)
import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Texture as Canvas
import Element exposing (..)
import Element.Font as Font
import Experiments.Raycast.Boundary as Boundary exposing (Boundary)
import Experiments.Raycast.Vehicle as Vehicle exposing (Vehicle)
import Html.Events.Extra.Mouse as Mouse
import Palette.Color exposing (..)
import Palette.Spacing exposing (..)
import Time exposing (posixToMillis)



-- MODULE PATTERN


type alias Module msg model =
    { view : model -> Element msg
    , update : Msg -> model -> ( model, Cmd msg )
    , init : ( Model, Cmd msg )
    , subscriptions : model -> Sub msg
    }


init :
    { toModel : model -> Model -> model
    , fromModel : model -> Model
    , toMsg : Msg -> msg
    }
    -> Module msg model
init { toModel, fromModel, toMsg } =
    let
        mapView : model -> Element msg
        mapView model =
            view (fromModel model)
                |> Element.map toMsg

        mapUpdate : Msg -> model -> ( model, Cmd msg )
        mapUpdate msg model =
            update msg (fromModel model)
                |> (\( formModel, formCmd ) ->
                        ( toModel model formModel
                        , Cmd.map toMsg formCmd
                        )
                   )
    in
    { view = mapView
    , update = mapUpdate
    , init =
        Tuple.mapSecond (Cmd.map toMsg) init_
    , subscriptions =
        \model ->
            subscriptions (fromModel model)
                |> Sub.map toMsg
    }



-- TYPES


type Model
    = Model
        { time : Float
        , walls : List Boundary
        , vehicle : Vehicle
        , mouse : Maybe ( Float, Float )
        }


type Msg
    = NoOp
    | AnimationFrame Time.Posix
    | MouseMoved ( Float, Float )



-- INIT, VIEW, UPDATE, SUBSCRIPTIONS


width : Int
width =
    1080


height : Int
height =
    720


init_ : ( Model, Cmd Msg )
init_ =
    ( Model
        { time = 0.0
        , walls =
            [ Boundary.create 300 40 320 180
            , Boundary.create 300 40 0 180
            , Boundary.create 0 180 320 180
            , Boundary.create 0 180 0 40
            , Boundary.create 320 180 320 0
            , Boundary.create 520 100 500 1000
            ]
                |> List.map (Boundary.moveBy 150 50)
        , vehicle = Vehicle.create 100 100
        , mouse = Nothing
        }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    column
        [ padding s2, spacing s2, Font.color white ]
        [ text "Hello, World!"
        , canvas model
        ]


canvas : Model -> Element Msg
canvas model =
    let
        canvasSize =
            ( width, height )
    in
    el [ centerX ] <|
        html <|
            Canvas.toHtml
                canvasSize
                [ Mouse.onMove (.offsetPos >> MouseMoved) ]
            <|
                (clearCanvas
                    :: raycast model
                )
                    ++ [ clearBottomHalf ]


clearCanvas : Canvas.Renderable
clearCanvas =
    Canvas.clear ( 0, 0 ) (toFloat width) (toFloat height)


clearBottomHalf : Canvas.Renderable
clearBottomHalf =
    Canvas.clear ( 0, toFloat height / 2 ) (toFloat width) (toFloat height / 2)


raycast : Model -> List Canvas.Renderable
raycast (Model { walls, vehicle }) =
    let
        boundaries_ =
            List.map Boundary.draw walls

        vehicle_ =
            Vehicle.drawTop walls vehicle
    in
    vehicle_ :: boundaries_


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model model) as model_) =
    case msg of
        NoOp ->
            ( model_, Cmd.none )

        AnimationFrame posixTime ->
            let
                time =
                    (toFloat <| posixToMillis posixTime) / 1000
            in
            ( Model { model | time = time }, Cmd.none )

        MouseMoved ( x, y ) ->
            let
                vehicle =
                    Vehicle.update x y model.vehicle
            in
            ( Model { model | mouse = Just ( x, y ), vehicle = vehicle }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame
