module Experiments.Raycast.Raycast01 exposing (Model, Module, Msg, init)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Texture as Canvas
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Events exposing (onMouseMove)
import Element.Font as Font
import Element.Input exposing (button)
import Experiments.Raycast.Boundary as Boundary exposing (Boundary)
import Experiments.Raycast.Ray as Ray exposing (Ray)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events.Extra.Mouse as Mouse
import List.Extra exposing (init)
import Palette.Color exposing (..)
import Palette.Navbar exposing (navbar)
import Palette.Spacing exposing (..)
import Time exposing (posixToMillis)
import Experiments.WFC.Canvas.Grid exposing (collapse)
import Experiments.WFC.Canvas.Tileset1 exposing (tiles)
import Experiments.WFC.Canvas.Types exposing (..)
import Experiments.WFC.Canvas.View exposing (board)



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
        , rays : List Ray
        , mouse : Maybe ( Float, Float )
        }


type Msg
    = NoOp
    | AnimationFrame Time.Posix
    | MouseMoved ( Float, Float )



-- INIT, VIEW, UPDATE, SUBSCRIPTIONS


init_ : ( Model, Cmd Msg )
init_ =
    ( Model
        { time = 0.0
        , walls = [ Boundary.create 300 40 320 180 ]
        , rays = [ Ray.create 0 90 ( 1, 0 ) ]
        , mouse = Nothing
        }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    column
        [ padding s16, spacing s4, Font.color white ]
        [ text "Hello, World!"
        , canvas model
        ]


canvas : Model -> Element Msg
canvas model =
    let
        canvasSize =
            ( 320, 180 )
    in
    el [ centerX ] <|
        html <|
            Canvas.toHtml
                canvasSize
                [ Mouse.onMove (.offsetPos >> MouseMoved) ]
            <|
                raycast model


raycast : Model -> List Canvas.Renderable
raycast (Model { walls, rays }) =
    let
        boundaries_ =
            List.map Boundary.draw walls

        rays_ =
            List.map Ray.debugDraw rays

        collisions =
            List.map (Ray.collisions walls) rays
                |> Debug.log "collided?"
    in
    boundaries_ ++ rays_


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
                a =
                    ( x, y ) |> Debug.log "mouse"

                rays_ =
                    model.rays
                        |> List.map (Ray.lookAt ( x, y ))
            in
            ( Model { model | mouse = Just ( x, y ), rays = rays_ }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame
