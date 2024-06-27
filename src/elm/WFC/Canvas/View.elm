module WFC.Canvas.View exposing (..)

import Canvas exposing (Renderable, group, text)
import Canvas.Settings exposing (Setting)
import Canvas.Settings.Advanced exposing (scale, transform, translate)
import Canvas.Texture as Canvas
import WFC.Canvas.Tileset1 exposing (openTile)
import WFC.Canvas.Types exposing (GridTile(..), Settings, Textures)


board : Textures -> Settings -> List GridTile -> Renderable
board textures settings grid =
    let
        tileSize =
            textures.blank
                |> Canvas.dimensions
                |> .width

        scale_ =
            settings.width / toFloat settings.dimensions / tileSize
    in
    grid
        |> List.indexedMap (tile textures settings)
        |> group [ transform [ scale scale_ scale_ ] ]


tile : Textures -> Settings -> Int -> GridTile -> Renderable
tile textures { dimensions, width, height } index tile_ =
    let
        tileSize =
            textures.blank
                |> Canvas.dimensions
                |> .width

        ( x, y ) =
            getXY index dimensions
                |> Tuple.mapBoth toFloat toFloat

        ( translateX, translateY ) =
            ( tileSize * x, tileSize * y )

        shape_ : Renderable
        shape_ =
            case tile_ of
                Open _ ->
                    openTile tileSize tileSize

                Collapsed tile__ ->
                    tile__.view textures
    in
    group
        [ transform [ translate translateX translateY ] ]
        [ shape_ ]


getXY : Int -> Int -> ( Int, Int )
getXY index dim =
    let
        x =
            remainderBy dim index

        y =
            index // dim
    in
    ( x, y )
