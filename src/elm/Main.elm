module Main exposing (main)

import Browser
import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Texture as Canvas
import Color
import Element exposing (..)
import Element.Font as Font
import Experiments.Raycast as Raycast
import Html exposing (Html)
import Palette.Navbar exposing (navbar)


main : Program {} Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { route : Route
    }


type Msg
    = NoOp
    | GotRaycastMsg Raycast.Msg


type Route
    = Raycast Raycast.Model


init flags =
    ( { route = Raycast <| Tuple.first raycastModule.init }
    , Cmd.none
    )



-- MODULES


raycastModule : Raycast.Module Msg Model
raycastModule =
    Raycast.init
        { toModel = \model rcModel -> { model | route = Raycast rcModel }
        , fromModel =
            \model ->
                case model.route of
                    Raycast rcModel ->
                        rcModel
        , toMsg = GotRaycastMsg
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotRaycastMsg rcMsg ->
            case model.route of
                Raycast rcModel ->
                    raycastModule.update rcMsg model



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill ]
    <|
        column
            [ width fill ]
            [ navbar
            , raycastModule.view model
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    raycastModule.subscriptions model
