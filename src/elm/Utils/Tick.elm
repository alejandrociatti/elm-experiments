module Utils.Tick exposing (..)

import Browser.Events exposing (onAnimationFrame)
import Time exposing (posixToMillis)


type Msg
    = InitTime Time.Posix
    | TickTime Time.Posix


type alias Model =
    { initial : Time.Posix
    , elapsed : Float
    }


init : Model
init =
    { initial = Time.millisToPosix 0
    , elapsed = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InitTime t ->
            { model | initial = t }

        TickTime t ->
            let
                timeInSeconds =
                    subtractTimeSeconds t model.initial
            in
            { model | elapsed = timeInSeconds }


subscription : Sub Msg
subscription =
    onAnimationFrame TickTime


subtractTimeSeconds : Time.Posix -> Time.Posix -> Float
subtractTimeSeconds t1 t0 =
    ((Basics.toFloat <| posixToMillis t1) - Basics.toFloat (posixToMillis t0)) / 1000
