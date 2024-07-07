module Main exposing (main)

import Browser
import Element exposing (..)
import Experiments.Raycast as Raycast
import Experiments.Snake as Snake
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
    | GotSnakeMsg Snake.Msg
    | ChangeRoute Route


type Route
    = Raycast Raycast.Model
    | Snake Snake.Model


init flags =
    -- ( { route = Raycast <| Tuple.first raycastModule.init }
    ( { route = Snake <| Tuple.first snakeModule.init }
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
                        Just rcModel

                    _ ->
                        Nothing
        , toMsg = GotRaycastMsg
        }


snakeModule : Snake.Module Msg Model
snakeModule =
    Snake.init
        { toModel = \model snakeModel -> { model | route = Snake snakeModel }
        , fromModel =
            \model ->
                case model.route of
                    Snake snakeModel ->
                        Just snakeModel

                    _ ->
                        Nothing
        , toMsg = GotSnakeMsg
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

                _ ->
                    ( model, Cmd.none )

        GotSnakeMsg snakeMsg ->
            case model.route of
                Snake snakeModel ->
                    snakeModule.update snakeMsg model

                _ ->
                    ( model, Cmd.none )

        ChangeRoute route ->
            ( { model | route = route }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill ]
    <|
        column
            [ width fill ]
            [ navbarWithRoutes model.route
            , case model.route of
                Raycast raycastModel ->
                    raycastModule.view model

                Snake snakeModel ->
                    snakeModule.view model
            ]


navbarWithRoutes : Route -> Element Msg
navbarWithRoutes route =
    case route of
        Raycast _ ->
            navbar <| Just (ChangeRoute (Snake <| Tuple.first snakeModule.init))

        Snake _ ->
            navbar <| Just (ChangeRoute (Raycast <| Tuple.first raycastModule.init))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ raycastModule.subscriptions model
        , snakeModule.subscriptions model
        ]
