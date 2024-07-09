module Main exposing (main)

import Browser
import Element exposing (..)
import Experiments.Landscape as Landscape
import Experiments.Raycast as Raycast
import Experiments.SnakeGame as Snake
import Html exposing (Html)
import Palette.Burger as Burger
import Palette.Navbar exposing (navbar)


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { route : Route
    , burger : Burger.Model Msg
    }


type Msg
    = NoOp
    | GotBurgerToggle Bool
    | GotLandscapeMsg Landscape.Msg
    | GotRaycastMsg Raycast.Msg
    | GotSnakeMsg Snake.Msg
    | ChangeRoute Route


type Route
    = Raycast Raycast.Model
    | Snake Snake.Model
    | Landscape Landscape.Model


init : {} -> ( Model, Cmd Msg )
init flags =
    -- ( { route = Raycast <| Tuple.first raycastModule.init }
    -- ( { route = Snake <| Tuple.first snakeModule.init
    ( { route = Landscape Landscape.init
      , burger = initBurger
      }
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

        GotBurgerToggle bool ->
            ( { model | burger = Burger.update bool model.burger }, Cmd.none )

        GotLandscapeMsg landscapeMsg ->
            case model.route of
                Landscape landscapeModel ->
                    ( { model | route = Landscape (Landscape.update landscapeMsg landscapeModel) }, Cmd.none )

                _ ->
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
            [ navbarWithRoutes model
            , case model.route of
                Raycast raycastModel ->
                    raycastModule.view model

                Snake snakeModel ->
                    snakeModule.view model

                Landscape landscape ->
                    html <| Landscape.view landscape
            ]


navbarWithRoutes : Model -> Element Msg
navbarWithRoutes model =
    let
        burger =
            Burger.view model.burger
    in
    navbar [ burger ]


initBurger : Burger.Model Msg
initBurger =
    Burger.init GotBurgerToggle
        [ Burger.menuItem "Landscape" (Just <| ChangeRoute (Landscape <| Landscape.init))
        , Burger.menuItem "Raycast" (Just <| ChangeRoute (Raycast <| Tuple.first raycastModule.init))
        , Burger.menuItem "Snake" (Just <| ChangeRoute (Snake <| Tuple.first snakeModule.init))
        , Burger.menuItemUrl "Wave Function C." "/wfc"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ raycastModule.subscriptions model
        , snakeModule.subscriptions model
        , Landscape.subscriptions |> Sub.map GotLandscapeMsg
        ]
