module WFC.Canvas.Types exposing (..)

import Canvas as Canvas
import Canvas.Settings exposing (Setting)
import Canvas.Texture as Canvas
import Json.Decode as D
import Time
import WFC.Common.Tile as Tile


type alias Tile =
    Tile.Tile Int (Textures -> Canvas.Renderable)


type GridTile
    = Collapsed Tile
    | Open (List Tile)


type alias Flags =
    List D.Value


type alias Settings =
    { width : Float
    , height : Float
    , dimensions : Int
    }


type Msg
    = NoOp
    | AnimationFrame Time.Posix
    | Reset
    | DimensionPlus
    | DimensionMinus


type alias Model =
    { time : Float
    , change : Bool
    , settings : Settings
    , grid : List GridTile
    , lastComputation : Float
    , textures : Maybe Textures
    }


type alias Textures =
    { blank : Canvas.Texture, down : Canvas.Texture }
