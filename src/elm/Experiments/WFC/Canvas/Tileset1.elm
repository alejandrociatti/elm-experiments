module Experiments.WFC.Canvas.Tileset1 exposing (openTile, tiles)

import Canvas exposing (Renderable, rect, shapes, texture)
import Canvas.Settings exposing (Setting, fill, stroke)
import Canvas.Texture exposing (Texture)
import Color
import Experiments.WFC.Canvas.Tile exposing (rotate)
import Experiments.WFC.Canvas.Types exposing (Textures, Tile)
import Experiments.WFC.Common.Tile exposing (Rule(..))


downTileView : Textures -> Renderable
downTileView textures =
    texture
        []
        ( 0, 0 )
        textures.down


blankTileView : Textures -> Renderable
blankTileView textures =
    texture
        []
        ( 0, 0 )
        textures.blank


openTile : Float -> Float -> Renderable
openTile width height =
    shapes
        [ fill Color.black ]
        [ rect ( 0, 0 ) width height ]


blankTile : Tile
blankTile =
    { rule = Symmetrical 0 0 0 0
    , view = blankTileView
    }


downTile : Tile
downTile =
    { rule = Symmetrical 0 1 1 1
    , view = downTileView
    }


tiles : List Tile
tiles =
    let
        -- blank, then T + its three rotations
        first =
            rotate downTile

        second =
            rotate first

        third =
            rotate second
    in
    [ blankTile
    , downTile
    , first
    , second
    , third
    ]
