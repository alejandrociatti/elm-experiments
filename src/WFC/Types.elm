module Types exposing (..)

import GraphicSVG.EllieApp exposing (GetKeyState)
import Tile exposing (Tile)

type Msg
    = NoOp
    | Tick Float GetKeyState

type GridTile
    = Collapsed (Tile Int Msg)
    | Open (List (Tile Int Msg))

type alias Model =
    { time : Float
    , change : Bool
    , settings : Settings
    , grid : List (GridTile)
    , lastComputation : Float
    -- , arrows : ( Float, Float )
    -- , wasd : ( Float, Float )
    }

type alias Settings =
    { width : Float 
    , height : Float
    , dimensions : Int
    }