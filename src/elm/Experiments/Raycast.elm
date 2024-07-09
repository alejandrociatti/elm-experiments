module Experiments.Raycast exposing (Model, Module, Msg, init)

import Browser.Events exposing (onAnimationFrame)
import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Texture as Canvas
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Events exposing (onMouseMove)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Experiments.Raycast.Boundary as Boundary exposing (Boundary)
import Experiments.Raycast.Camera as Vehicle exposing (Vehicle)
import Experiments.Raycast.Ray as Ray exposing (Ray)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Mouse
import List.Extra exposing (init)
import Palette.Border as Border
import Palette.Color as Color exposing (..)
import Palette.Navbar exposing (navbar)
import Palette.Spacing exposing (..)
import Time exposing (posixToMillis)
import Utils.Canvas exposing (transformAndRender)
import Utils.Random exposing (probabilityFromTime)



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

        unmapUpdate : model -> (Model, Cmd Msg) -> (model, Cmd msg)
        unmapUpdate userModel (innerModel, innerCmd) =
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


type Model
    = Model
        { time : Float
        , walls : List Boundary
        , vehicle : Vehicle
        , mouse : Maybe ( Float, Float )
        , projection : Bool
        }


type Msg
    = NoOp
    | AnimationFrame Time.Posix
    | MouseMoved ( Float, Float )
    | MouseWheelMoved Float
    | ChangeFov Int
    | RandomizeBounds
    | ToggleProjection



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
        , projection = False
        }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    column
        [ Element.width fill, spacing s2, Font.color white ]
        [ canvas model
        ]


canvas : Model -> Element Msg
canvas ((Model model_) as model) =
    let
        canvasSize =
            ( width, height )
    in
    el
        [ paddingXY s8 0
        , centerX
        , onLeft <| leftExplanationAndSettings model_.projection model_.vehicle.fov
        ]
    <|
        html <|
            Canvas.toHtml
                canvasSize
                [ Mouse.onMove (.offsetPos >> MouseMoved)
                , Mouse.onWheel (.deltaY >> MouseWheelMoved)
                ]
            <|
                clearCanvas
                    :: raycast model


clearCanvas : Canvas.Renderable
clearCanvas =
    Canvas.clear ( 0, 0 ) (toFloat width) (toFloat height)


clearBottomHalf : Canvas.Renderable
clearBottomHalf =
    Canvas.clear ( 0, toFloat height / 2 ) (toFloat width) (toFloat height / 2)


raycast : Model -> List Canvas.Renderable
raycast (Model { walls, vehicle, projection }) =
    let
        boundaries_ =
            List.map Boundary.draw walls

        topView =
            Vehicle.drawTop walls vehicle

        frontView =
            [ clearBottomHalf
            , transformAndRender
                ( width, height )
                ( 0, toFloat height / 2 )
                ( toFloat width, toFloat height / 2 )
              <|
                Vehicle.drawFOV projection width height walls vehicle
            ]
    in
    topView :: boundaries_ ++ clearBottomHalf :: frontView


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
                    Vehicle.move x y model.vehicle
            in
            ( Model { model | mouse = Just ( x, y ), vehicle = vehicle }
            , Cmd.none
            )

        MouseWheelMoved delta ->
            let
                vehicle =
                    Vehicle.rotate (model.vehicle.direction + round delta) model.vehicle
            in
            ( Model { model | vehicle = vehicle }, Cmd.none )

        ChangeFov fov ->
            let
                vehicle_ =
                    Vehicle.changeFov fov model.vehicle
            in
            ( Model { model | vehicle = vehicle_ }, Cmd.none )

        RandomizeBounds ->
            let
                probability =
                    probabilityFromTime model.time

                walls_ =
                    List.range 0 (5 + probability // 5)
                        |> List.map
                            (\n ->
                                let
                                    x1 =
                                        probabilityFromTime (model.time + toFloat n / 100)
                                            |> toFloat
                                            |> (*) (toFloat width / 100)

                                    x2 =
                                        probabilityFromTime (model.time + toFloat n / 200)
                                            |> toFloat
                                            |> (*) (toFloat width / 100)

                                    y1 =
                                        probabilityFromTime (model.time + toFloat n / 300)
                                            |> toFloat
                                            |> (*) (toFloat height / 100)

                                    y2 =
                                        probabilityFromTime (model.time + toFloat n / 400)
                                            |> toFloat
                                            |> (*) (toFloat height / 100)
                                in
                                Boundary.create x1 y1 x2 y2
                            )
            in
            ( Model { model | walls = walls_ }, Cmd.none )

        ToggleProjection ->
            ( Model { model | projection = not model.projection }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame


leftExplanationAndSettings : Bool -> Int -> Element Msg
leftExplanationAndSettings projection fov =
    column
        [ Element.width (fill |> minimum s16 |> maximum 300), padding s2, spacing s2 ]
        [ column
            [ Font.color white
            , Font.size s2_5
            , spacing s1
            ]
            [ paragraph [ Font.bold, Font.size s3 ] [ text "random map with raycast representation (top & pov)" ]
            , bulletedItem "move mouse to move camera"
            , bulletedItem "scroll to rotate camera"
            , bulletedItem "change fov with slider"
            ]
        , Input.slider
            [ Element.height <| px 30
            , behindContent
                (el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , centerY
                    , Background.color Color.gray35
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = round >> ChangeFov
            , label =
                Input.labelAbove []
                    (text <| "Field Of View (" ++ String.fromInt fov ++ "°)")
            , min = 45
            , max = 360
            , step = Just 5
            , value = toFloat fov
            , thumb =
                Input.defaultThumb
            }
        , Input.button
            [ Element.width (fill |> minimum s16 |> maximum 300)
            , Element.height (px 30)
            , paddingXY s2 s4
            , Font.size s2
            , Font.color white
            , Background.color Color.gray35
            , Border.rounded 2
            ]
            { onPress = Just <| ChangeFov 60
            , label = text "reset field-of-view"
            }
        , Input.button
            [ Element.width (fill |> minimum s16 |> maximum 300)
            , Element.height (px 30)
            , paddingXY s2 s4
            , Font.size s2
            , Font.color white
            , Background.color Color.gray35
            , Border.rounded 2
            ]
            { onPress = Just <| RandomizeBounds
            , label = text "randomize bounds"
            }
        , toggle projection
        ]


bulletedItem : String -> Element msg
bulletedItem item =
    row [ spacing s1, Font.size s2 ]
        [ el [ Font.bold ] (text "•")
        , el [] (text item)
        ]


toggle projection =
    let
        attrs isCurrent =
            if isCurrent then
                [ Font.color white
                , Background.color Color.gray35
                ]

            else
                [ Font.color Color.gray35
                , Background.color black
                ]
    in
    row []
        [ Input.button
            ([ Element.width fill
             , Element.height (px 30)
             , paddingXY s2 s4
             , Font.size s2
             , Border.roundLeft 2
             ]
                ++ attrs (not projection)
            )
            { onPress = Just <| ToggleProjection
            , label = text "euclidean dist."
            }
        , Input.button
            ([ Element.width fill
             , Element.height (px 30)
             , paddingXY s2 s4
             , Font.size s2
             , Border.roundLeft 2
             ]
                ++ attrs projection
            )
            { onPress = Just <| ToggleProjection
            , label = text "projection dist."
            }
        ]
