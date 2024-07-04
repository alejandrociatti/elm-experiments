module Utils.Keyboard exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as D


type alias Key =
    Int


w : Key
w =
    87


s : Key
s =
    83


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keyup" (D.map tagger keyCode)


keyCodeFromString : String -> Int
keyCodeFromString key =
    case key of
        "a" ->
            65

        "q" ->
            81

        "e" ->
            69

        "Enter" ->
            13

        "ArrowUp" ->
            38

        "ArrowDown" ->
            40

        "ArrowLeft" ->
            37

        "ArrowRight" ->
            39

        _ ->
            0
