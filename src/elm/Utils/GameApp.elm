module Utils.GameApp exposing (GameModel, GetKeyState, InputHandler, KeyState(..), Keys(..), Model, Msg, init, subscriptions, update, view)

import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Canvas
import Dict
import Element exposing (..)
import Element.Font as Font
import Json.Decode as D
import Palette.Color exposing (white)
import Palette.Spacing exposing (..)
import Palette.Utils exposing (attributeNone)
import Task
import Time exposing (millisToPosix, posixToMillis)


init : Int -> Int -> InputHandler msg -> Model msg
init width height tick =
    { tick = tick
    , keys = Dict.empty
    , initT = millisToPosix 0
    , width = width
    , height = height
    , onLeft = Nothing
    , onRight = Nothing
    }


{-| The `InputHandler` type alias descripts a message that contains a Float representing the time in seconds from
the time the program started and the `GetKeyState` type alias used for returning key actions.
This type is used for by the `gameApp` fully interactive application.
-}
type alias InputHandler userMsg =
    Float
    -> GetKeyState
    -> userMsg


{-| `GetKeyState` returns a triple where the first argument is of type `Keys -> KeyState`
so you can ask if a certain key is pressed. The other two are tuples of arrow keys and
WASD keys, respectively. They're in the form (x,y) which represents the key presses
of each player. For example, (0,-1) represents the left arrow (or "A") key, and (1,1)
would mean the up (or "W") and right (or "D") key are being pressed at the same time.
-}
type alias GetKeyState =
    ( Keys -> KeyState, ( Float, Float ), ( Float, Float ) )


type alias KeyCode =
    Int


type alias KeyDict =
    Dict.Dict KeyCode ( KeyState, Bool )


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


subscriptions : List (Sub (Msg userMsg))
subscriptions =
    [ onKeyUp (D.map KeyUp (D.field "keyCode" D.int))
    , onKeyDown (D.map KeyDown (D.field "keyCode" D.int))
    , onAnimationFrame TickTime
    ]


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


subtractTimeSeconds : Time.Posix -> Time.Posix -> Float
subtractTimeSeconds t1 t0 =
    ((Basics.toFloat <| posixToMillis t1) - Basics.toFloat (posixToMillis t0)) / 1000


type Msg userMsg
    = InitTime Time.Posix
    | TickTime Time.Posix
    | KeyDown Int
    | KeyUp Int



-- type alias GameApp flags model msg =
-- Program flags (Model model msg) (Msg msg)


type alias GameModel model msg =
    ( Model msg
    , model
    )


type alias Model msg =
    { tick : InputHandler msg
    , keys : KeyDict
    , initT : Time.Posix
    , width : Int
    , height : Int
    , onLeft : Maybe (Element msg)
    , onRight : Maybe (Element msg)
    }


update :
    (msg -> model -> model)
    -> Msg msg
    -> GameModel model msg
    -> ( GameModel model msg, Cmd (Msg msg) )
update userUpdate msg (( model, _ ) as models) =
    case msg of
        InitTime t ->
            ( Tuple.mapFirst (always { model | initT = t }) models, Cmd.none )

        TickTime t ->
            ( models, Cmd.none )

        KeyDown keyCode ->
            ( Tuple.mapFirst
                (always { model | keys = insertKeyDict model.keys keyCode WentDown })
                models
            , Cmd.none
            )

        KeyUp keyCode ->
            ( Tuple.mapFirst
                (always { model | keys = insertKeyDict model.keys keyCode WentUp })
                models
            , Cmd.none
            )


view : GameModel model msg -> List Canvas.Renderable -> Element msg
view model renderables =
    column
        [ Element.width fill, spacing s2, Font.color white ]
        [ canvas model renderables
        ]


canvas : GameModel model msg -> List Canvas.Renderable -> Element msg
canvas ( { width, height } as model, userModel ) renderables =
    let
        canvasSize =
            ( width, height )
    in
    el
        [ paddingXY s8 0
        , centerX
        , maybeOnLeft model.onLeft
        , maybeOnRight model.onRight
        ]
    <|
        html <|
            Canvas.toHtml
                canvasSize
                []
            <|
                clearCanvas model
                    :: renderables


clearCanvas : Model msg -> Canvas.Renderable
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
