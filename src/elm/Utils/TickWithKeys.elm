module Utils.TickWithKeys exposing (..)

import Utils.Keyboard as Keyboard exposing (GetKeyState, Keys(..))
import Utils.Tick as Tick


type alias Model =
    { time : Tick.Model
    , keys : Keyboard.Model
    , state : GetKeyState
    }


type Msg
    = TickMsg Tick.Msg
    | KeyMsg Keyboard.Msg


init : Model
init =
    let
        initialKeys =
            Keyboard.init
    in
    { time = Tick.init
    , keys = initialKeys
    , state = getKeyState initialKeys
    }


toTickMsg : Model -> (Float -> GetKeyState -> msg) -> msg
toTickMsg model msg =
    msg model.time.elapsed model.state


getKeyState : Keyboard.Model -> GetKeyState
getKeyState keys =
    let
        keyChecker =
            Keyboard.keyCheckerFunction keys

        arrowKeys =
            Keyboard.arrowChecker keyChecker UpArrow DownArrow LeftArrow RightArrow

        wasd =
            Keyboard.arrowChecker keyChecker (Key "w") (Key "s") (Key "a") (Key "d")
    in
    ( keyChecker, arrowKeys, wasd )


update : Msg -> Model -> Model
update msg model =
    case msg of
        TickMsg tickMsg ->
            let
                time =
                    Tick.update tickMsg model.time

                keys =
                    Keyboard.maintainKeyDict model.keys
            in
            { model | time = time, keys = keys, state = getKeyState keys }

        KeyMsg keyMsg ->
            { model | keys = Keyboard.update keyMsg model.keys }


subscriptions : List (Sub Msg)
subscriptions =
    (Tick.subscription |> Sub.map TickMsg)
        :: (Keyboard.subscriptions |> List.map (Sub.map KeyMsg))
