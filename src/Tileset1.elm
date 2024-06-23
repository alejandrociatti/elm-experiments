module Tileset1 exposing (tiles, openTile)

import GraphicSVG exposing (Shape, rect, html, filled, black, addOutline, solid, white)
import Html.Attributes as HA
import Html exposing (img)
import Tile exposing (Tile)
import Tile exposing (Rule(..))
import Tile exposing (rotate)

downTile : Shape msg
downTile = 
    html 50.0 50.0 <| img [ HA.src "/down.png" ] []

blankTile : Shape msg
blankTile = 
    html 50.0 50.0 <| img [ HA.src "/blank.png" ] []

openTile : Shape msg
openTile = 
    rect 50.0 50.0
        |> filled black
        |> addOutline (solid 1) white


blank : Tile Int msg
blank =
    { rule = Symmetrical 0 0 0 0
    , view = blankTile
    }

down : Tile Int msg
down =
    { rule = Symmetrical 0 1 1 1
    , view = downTile
    }

tiles : List (Tile Int msg)
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