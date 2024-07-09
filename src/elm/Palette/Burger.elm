module Palette.Burger exposing (Model, init, menuItem, menuItemUrl, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Html exposing (Html)
import Json.Decode exposing (maybe)
import Palette.Color exposing (..)
import Palette.Spacing exposing (..)
import Palette.Utils exposing (attributeNone, ellipsis)



-- MODEL


type alias Model msg =
    { menuOpen : Bool
    , menuItems : List (Element msg)
    , toggleMsg : Bool -> msg
    }


w : number
w =
    180


init : (Bool -> msg) -> List (Element msg) -> Model msg
init toggleTagger items =
    { menuOpen = False
    , menuItems = items
    , toggleMsg = toggleTagger
    }


update : Bool -> Model msg -> Model msg
update isOpen model =
    { model | menuOpen = isOpen }



-- VIEW


view : Model msg -> Element msg
view model =
    column [ centerX, width fill, height fill, pointer ]
        [ hamburgerButton model
        ]


hamburgerButton : Model msg -> Element msg
hamburgerButton { menuOpen, menuItems, toggleMsg } =
    el
        [ onClick (toggleMsg <| not menuOpen)
        , alignRight
        , padding 10
        , Border.rounded 5
        , Background.color gray35
        , if menuOpen then
            below <| menu menuItems

          else
            attributeNone
        ]
        (text
            (if menuOpen then
                "✕"

             else
                "☰"
            )
        )


menu : List (Element msg) -> Element msg
menu items =
    column
        [ width (fill |> minimum w)
        , Background.color gray35
        , alignLeft
        , moveLeft (w - 37)
        ]
        items


menuItem : String -> Maybe msg -> Element msg
menuItem label maybeMsg =
    let
        msg =
            case maybeMsg of
                Just m ->
                    onClick m

                Nothing ->
                    attributeNone
    in
    el
        [ width fill
        , padding s2
        , pointer
        , msg
        , mouseOver [ Background.color gray43 ]
        ]
        (text label)


menuItemUrl : String -> String -> Element msg
menuItemUrl label url =
    link
        [ width (px w)
        , padding s2
        , pointer
        , mouseOver [ Background.color gray43 ]
        ]
        { url = url, label = ellipsis label }
