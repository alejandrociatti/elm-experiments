module MainFromJs exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Texture as Texture exposing (Source, Texture)
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import String exposing (right)


-- MAIN


main : Program Flags Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { tiles : List Tile
    }

type alias Flags =
    { blank : Value 
    , down : Value
    , up : Value
    , left : Value
    , right : Value
    }


type alias Tile =
    { image : String
    , texture : Maybe Texture
    , textureSource : Value
    , kind : TileKind
    }


type TileKind
    = Blank
    | DownT
    | LeftT
    | RightT
    | UpT


tileKinds : List TileKind
tileKinds =
    [ Blank, DownT, LeftT, RightT, UpT ]


kindToString : TileKind -> String
kindToString kind =
    case kind of
        Blank ->
            "blank"

        DownT ->
            "down"

        LeftT ->
            "left"

        RightT ->
            "right"

        UpT ->
            "up"


init : Flags -> (Model, Cmd Msg) 
init flags =
    let
        base =
            "/public/"

        getValue : TileKind -> Value
        getValue kind =
            case kind of
                Blank ->
                    flags.blank

                DownT ->
                    flags.down

                LeftT ->
                    flags.left

                RightT ->
                    flags.right

                UpT ->
                    flags.up

        tiles =
            tileKinds
                |> List.map
                    (\kind ->
                        let
                            image =
                                base ++ kindToString kind ++ ".png"
                        in
                        { image = image
                        , texture = Nothing
                        , textureSource = getValue kind
                        , kind = kind
                        }
                    )
    in
   ( { tiles = tiles }
   , Cmd.none
   )



-- UPDATE


type Msg
    = NoOp 


update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
    let
        width =
            600

        height =
            600
    in
    Canvas.toHtml (width, height) 
        [ style "border" "1px solid black" ]
        ([ shapes [ fill Color.white ]
            [ rect ( 0, 0 ) width height
            ]
         , renderSquare
         ]
            ++ (case List.head model.tiles |> Maybe.map .textureSource |> Maybe.andThen Texture.fromDomImage of
                    Just tileTexture ->
                        let
                            d = tileTexture |> Debug.log "texture locked in"
                        in
                        [ texture [] ( 0, 0 ) tileTexture ]

                    _ ->
                        []
               )
        )


renderSquare : Renderable
renderSquare =
    shapes
        [ fill <| Color.rgba 0 0 0 1 ]
        [ rect ( 0, 0 ) 100 5 ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none