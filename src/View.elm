module View exposing (..)

import GraphicSVG exposing (Shape, scale, move, group)
import Tileset1 exposing (openTile)
import Types exposing (Msg, Settings)
import Types exposing (GridTile)
import Types exposing (GridTile(..))


board : Settings -> List GridTile -> Shape Msg
board settings grid =
    let
        dimensions = 
            settings.dimensions 

    in
    grid 
        |> List.indexedMap (tile settings)
        |> group
        |> scale 0.8
        |> move (-settings.width / 3, settings.height/3)


tile : Settings -> Int -> GridTile -> Shape Msg 
tile {dimensions, width, height} index tile_ =
    let
        tileSize = 50.0

        scale_ = width / toFloat dimensions / tileSize

        finalSize = tileSize * scale_

        (x, y) = getXY index dimensions |> Tuple.mapBoth toFloat toFloat

        shape_ =
            case tile_ of
                Open _ ->
                    openTile

                Collapsed tile__ ->
                    tile__.view

                
    in
    shape_ 
        |> scale scale_
        |> move  (finalSize * x, -finalSize * y)


getXY : Int -> Int -> (Int, Int)
getXY index dim =
    let
        x = remainderBy dim index
        y = index // dim
    in
    (x, y)

