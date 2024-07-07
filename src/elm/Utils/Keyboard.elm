module Utils.Keyboard exposing (GetKeyState, KeyState(..), Keys(..), Model, Msg, arrowChecker, init, keyCheckerFunction, maintainKeyDict, subscriptions, update)

import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict
import Json.Decode as D


type Msg
    = KeyDown Int
    | KeyUp Int


type alias GetKeyState =
    ( Keys -> KeyState, ( Float, Float ), ( Float, Float ) )


type alias KeyCode =
    Int


type alias KeyDict =
    Dict.Dict KeyCode ( KeyState, Bool )


type alias Model =
    KeyDict


{-| The possible states when you ask for a key's state:

  - `JustDown` is the frame after the key went down (will show up exactly once per press)
  - `Down` is a press that is continuing for more than one frame
  - `JustUp` is the frame after the key went up / stopped being pressed (will show up exactly once per press)
  - `Up` means the key is not currently being pressed nor was it recently released

-}
type KeyState
    = JustDown
    | Down
    | JustUp
    | Up


type KeyAction
    = WentUp
    | WentDown


{-| Includes all the regular keys. Ask for letters and numbers using `Key String`, e.g. `Key "a"` or `Key "3"`.
-}
type Keys
    = Key String
    | Backspace
    | Tab
    | Enter
    | Shift
    | Ctrl
    | Alt
    | Caps
    | LeftArrow
    | UpArrow
    | RightArrow
    | DownArrow
    | Delete
    | Space


subscriptions : List (Sub Msg)
subscriptions =
    [ onKeyUp (D.map KeyUp (D.field "keyCode" D.int))
    , onKeyDown (D.map KeyDown (D.field "keyCode" D.int))
    ]


init : Model
init =
    Dict.empty


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyDown keyCode ->
            insertKeyDict model keyCode WentDown

        KeyUp keyCode ->
            insertKeyDict model keyCode WentUp


insertKeyDict : KeyDict -> KeyCode -> KeyAction -> KeyDict
insertKeyDict dict key action =
    let
        currState =
            Dict.get key dict
    in
    case currState of
        Just ( JustDown, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustDown, False )

                    WentUp ->
                        ( JustDown, True )
                )
                dict

        Just ( Down, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( Down, False )

                    WentUp ->
                        ( JustUp, False )
                )
                dict

        Just ( Up, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustDown, False )

                    WentUp ->
                        ( JustUp, False )
                )
                dict

        Just ( JustUp, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustUp, True )

                    WentUp ->
                        ( JustUp, False )
                )
                dict

        Just ( state, True ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( state, True )

                    WentUp ->
                        ( state, True )
                )
                dict

        Nothing ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustDown, False )

                    WentUp ->
                        ( JustUp, False )
                )
                dict


maintainKeyDict : KeyDict -> KeyDict
maintainKeyDict dict =
    Dict.filter filterHelper (Dict.map maintainHelper dict)


filterHelper : a -> ( KeyState, b ) -> Bool
filterHelper key action =
    case action of
        ( Up, _ ) ->
            False

        _ ->
            True


maintainHelper : a -> ( KeyState, Bool ) -> ( KeyState, Bool )
maintainHelper key action =
    case action of
        ( JustUp, False ) ->
            ( Up, False )

        ( JustUp, True ) ->
            ( JustDown, False )

        ( Up, False ) ->
            ( Up, False )

        ( Up, True ) ->
            ( Up, False )

        ( JustDown, False ) ->
            ( Down, False )

        ( JustDown, True ) ->
            ( JustUp, False )

        ( Down, False ) ->
            ( Down, False )

        ( Down, True ) ->
            ( Down, False )


keyCheckerFunction : Dict.Dict Int ( KeyState, a ) -> Keys -> KeyState
keyCheckerFunction dict key =
    let
        state =
            Dict.get kc dict

        kc =
            case key of
                Key str ->
                    Char.toCode <|
                        Char.toUpper <|
                            case String.uncons str of
                                Just ( a, _ ) ->
                                    a

                                Nothing ->
                                    'z'

                Backspace ->
                    8

                Tab ->
                    9

                Enter ->
                    13

                Shift ->
                    16

                Ctrl ->
                    17

                Alt ->
                    18

                Caps ->
                    20

                Space ->
                    32

                LeftArrow ->
                    37

                UpArrow ->
                    38

                RightArrow ->
                    39

                DownArrow ->
                    40

                Delete ->
                    46
    in
    case state of
        Just ( JustDown, _ ) ->
            JustDown

        Just ( Down, _ ) ->
            Down

        Just ( JustUp, _ ) ->
            JustUp

        Just ( Up, _ ) ->
            Up

        Nothing ->
            Up


arrowChecker :
    (Keys -> KeyState)
    -> Keys
    -> Keys
    -> Keys
    -> Keys
    -> ( number, number )
arrowChecker checker up down left right =
    ( case ( checker left, checker right ) of
        ( Down, Up ) ->
            -1

        ( Down, JustUp ) ->
            -1

        ( JustDown, Up ) ->
            -1

        ( JustDown, JustUp ) ->
            -1

        ( Up, Down ) ->
            1

        ( JustUp, Down ) ->
            1

        ( Up, JustDown ) ->
            1

        ( JustUp, JustDown ) ->
            1

        _ ->
            0
    , case ( checker down, checker up ) of
        ( Down, Up ) ->
            1

        ( Down, JustUp ) ->
            1

        ( JustDown, Up ) ->
            1

        ( JustDown, JustUp ) ->
            1

        ( Up, Down ) ->
            -1

        ( JustUp, Down ) ->
            -1

        ( Up, JustDown ) ->
            -1

        ( JustUp, JustDown ) ->
            -1

        _ ->
            0
    )
