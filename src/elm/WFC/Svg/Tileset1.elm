module WFC.Svg.Tileset1 exposing (openTile, tiles)

import GraphicSVG exposing (Shape, addOutline, black, filled, html, move, rect, solid, white)
import Html exposing (img)
import Html.Attributes as HA
import WFC.Common.Tile exposing (Rule(..))
import WFC.Svg.Tile exposing (rotate)
import WFC.Svg.Types exposing (Msg, Tile)


downTile : Shape Msg
downTile =
    (html 50.0 50.0 <| img [ HA.src "assets/down.png" ] [])
        |> move ( -50 / 2, 50 / 2 )


blankTile : Shape Msg
blankTile =
    (html 50.0 50.0 <| img [ HA.src "assets/blank.png" ] [])
        |> move ( -50 / 2, 50 / 2 )


openTile : Shape Msg
openTile =
    rect 50.0 50.0
        |> filled black
        |> addOutline (solid 1) white


blank : Tile
blank =
    { rule = Symmetrical 0 0 0 0
    , view = blankTile
    }


down : Tile
down =
    { rule = Symmetrical 0 1 1 1
    , view = downTile
    }


tiles : List Tile
tiles =
    let
        -- blank, then T + its three rotations
        first =
            rotate down

        second =
            rotate first

        third =
            rotate second
    in
    [ blank
    , down
    , first
    , second
    , third
    ]
