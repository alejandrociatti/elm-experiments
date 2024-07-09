module Utils.Layout exposing (..)

import Canvas as Canvas
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Events exposing (on)
import Html.Attributes as HA
import Palette.Color exposing (..)
import Palette.Spacing exposing (..)
import Palette.Utils exposing (attributeNone)


type alias Settings msg =
    { width : Int
    , height : Int
    , onLeft : Maybe (Element msg)
    , onRight : Maybe (Element msg)
    }


view : Settings msg -> List Canvas.Renderable -> Element msg
view settings renderables =
    column
        [ Element.width fill
        , Background.color gray35
        , Font.color white
        , paddingXY 0 s10
        , spacing s2
        ]
        [ canvas settings renderables
        ]


canvas : Settings msg -> List Canvas.Renderable -> Element msg
canvas settings renderables =
    let
        canvasSize =
            ( settings.width, settings.height )
    in
    el
        [ paddingXY s8 0
        , centerX
        , maybeOnLeft settings.onLeft
        , maybeOnRight settings.onRight
        ]
    <|
        html <|
            Canvas.toHtml
                canvasSize
                [HA.style "background-color" "black"]
            <|
                clearCanvas settings
                    :: renderables


clearCanvas : Settings msg -> Canvas.Renderable
clearCanvas { width, height } =
    Canvas.clear ( 0, 0 ) (toFloat width) (toFloat height)


maybeOnLeft : Maybe (Element msg) -> Attribute msg
maybeOnLeft maybeElement =
    case maybeElement of
        Just element ->
            onLeft element

        Nothing ->
            attributeNone


maybeOnRight : Maybe (Element msg) -> Attribute msg
maybeOnRight maybeElement =
    case maybeElement of
        Just element ->
            onRight element

        Nothing ->
            attributeNone
