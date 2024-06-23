module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Texture as Texture exposing (Source, Texture)
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Canvas.Texture exposing (dimensions)
import Canvas.Settings.Advanced exposing (transform, translate, scale, imageSmoothing)
import Html.Attributes as Attributes
import Canvas.Settings.Text as Text 



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { tiles : List Tile
    }


type alias Tile =
    { image : String
    , texture : Maybe Texture
    , textureSource : Source Msg
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


init : Model
init =
    let
        base =
            "http://localhost:5173/"

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
                        , textureSource = Texture.loadFromImageUrl image <| GotTexture kind
                        , kind = kind
                        }
                    )
    in
    { tiles = tiles }



-- UPDATE


type Msg
    = GotTexture TileKind (Maybe Texture)


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotTexture kind maybeTexture ->
            { model
                | tiles =
                    model.tiles
                        |> List.map
                            (\tile ->
                                if tile.kind == kind then
                                    { tile | texture = maybeTexture }

                                else
                                    tile
                            )
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        width =
            1200

        height =
            1200

        settings = { width = width, height = height, textures = List.map .textureSource model.tiles } 
    in
    Html.div 
        [] 
        [ Canvas.toHtmlWith settings 
            [ Attributes.style "border" "2px solid red" ]
            (shapes [ fill Color.white ] [ rect ( 50, 50 ) width height ]
                :: List.indexedMap
                    (renderTile width height) 
                    model.tiles 
            ) 
        , Canvas.toHtml (width, height) 
            [ Attributes.style "border" "2px solid red" ]
            [ Canvas.text
                [ Text.font { size = 48, family = "sans-serif" }
                , Text.align Text.Center
                , stroke Color.white
                ]
                ( 50, 50 )
                "Hello world"
            , shapes [ fill Color.white ] [ rect ( 5, 5 ) (width / 2 ) (height / 2) ]
            ]
        , Html.img 
            [ Attributes.src "/down.png" ]
            []
        ]
    


renderTile : Int -> Int -> Int -> Tile -> Renderable
renderTile w h i t =
    case t.texture of
        Just texture ->
            let
                d =
                    dimensions texture 
                       |> Debug.log "texture dimension"

                

                total = toFloat 5

                maxSize = toFloat w / total

                ( x, y ) =
                    ( (toFloat w / total) * toFloat i
                    , toFloat <| i * 50 
                    )

                ( ratio, smoothing ) =
                    if i == round total - 1 then
                        ( 7.5, False )

                    else
                        ( maxSize / max d.width d.height, True )
            in
            Canvas.texture
                [ transform [ translate x y, scale ratio ratio ]
                , imageSmoothing smoothing
                ]
                ( 0, 0 )
                texture

        Nothing ->
            shapes [ fill Color.black ] [ rect ( 0, 0 ) 100 5 ]
