module WFC.Svg.Types exposing (..)

import GraphicSVG exposing (Shape)
import GraphicSVG.EllieApp exposing (GetKeyState)
import WFC.Common.Tile as Tile


type alias Tile =
    Tile.Tile Int (Shape Msg)


type GridTile
    = Collapsed Tile
    | Open (List Tile)


type alias Flags =
    { blank : String
    , down : String
    }


type alias Settings =
    { width : Float
    , height : Float
    , dimensions : Int
    }


type Msg
    = NoOp
    | Tick Float GetKeyState


type alias Model =
    { time : Float
    , change : Bool
    , settings : Settings
    , grid : List GridTile
    , lastComputation : Float

    -- , arrows : ( Float, Float )
    -- , wasd : ( Float, Float )
    }
