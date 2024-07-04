module Sandbox exposing (main)

import Browser
import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Texture as Canvas
import Color
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html, canvas)


width_ : Int
width_ =
    600


height_ : Int
height_ =
    600


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { blankTextureSource : String
    , downTextureSource : String
    , blankTexture : Maybe Canvas.Texture
    , downTexture : Maybe Canvas.Texture
    }


type alias Flags =
    { blank : String
    , down : String
    }


type Msg
    = NoOp
    | GotBlankTexture (Maybe Canvas.Texture)
    | GotDownTexture (Maybe Canvas.Texture)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { blankTextureSource = flags.blank
      , downTextureSource = flags.down
      , blankTexture = Nothing
      , downTexture = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotBlankTexture maybeTexture ->
            ( { model | blankTexture = maybeTexture }
            , Cmd.none
            )

        GotDownTexture maybeTexture ->
            ( { model | downTexture = maybeTexture }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        canvasSettings =
            { width = width_
            , height = height_
            , textures = [ Canvas.loadFromImageUrl model.blankTextureSource GotBlankTexture, Canvas.loadFromImageUrl model.downTextureSource GotDownTexture ]
            }
    in
    layout [ width fill ]
        (column
            [ padding 24, centerX, centerY, height fill, width fill ]
            [ el
                [ spacing 24, Font.size 24, Font.bold, centerX, spacing 20, Font.color <| rgb 255 255 255 ]
                (text "Hello, elm-ui!")
            , column
                [ width fill, centerX, centerY, spacing 24 ]
                [ html <|
                    Canvas.toHtmlWith canvasSettings [] <|
                        clearScreen
                            :: drawBoard model.blankTexture model.downTexture
                ]
            ]
        )


drawBoard : Maybe Canvas.Texture -> Maybe Canvas.Texture -> List Canvas.Renderable
drawBoard maybeBlankTexture maybeDownTexture =
    let
        initialShapes =
            Canvas.shapes
                [ Canvas.fill <| Color.rgb 0.8 0.8 0.8 ]
                [ Canvas.rect ( 0, 0 ) 100 100
                ]
    in
    case ( maybeBlankTexture, maybeDownTexture ) of
        ( Just blankTexture, Just downTexture ) ->
            [ initialShapes
            , Canvas.texture [] ( 0, 0 ) blankTexture
            , Canvas.texture [] ( 50, 50 ) downTexture
            , Canvas.texture [] ( 100, 100 ) downTexture
            , Canvas.group
                [ Canvas.transform [ Canvas.translate 175 175, Canvas.rotate (degrees -90), Canvas.translate -175 -175 ] ]
                [ Canvas.texture [] ( 150, 150 ) downTexture ]
            , Canvas.texture [] ( 200, 200 ) downTexture
            ]

        _ ->
            [ initialShapes ]


clearScreen : Canvas.Renderable
clearScreen =
    Canvas.shapes
        [ Canvas.fill <| Color.rgb 0.2 0.2 0.2 ]
        [ Canvas.rect ( 0, 0 ) (toFloat width_) (toFloat height_) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
