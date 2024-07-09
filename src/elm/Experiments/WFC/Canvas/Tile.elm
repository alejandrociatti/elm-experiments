module Experiments.WFC.Canvas.Tile exposing (..)

import Canvas as Canvas
import Canvas.Settings exposing (Setting)
import Canvas.Settings.Advanced as Canvas
import Canvas.Texture as Canvas
import Experiments.WFC.Canvas.Types exposing (Textures, Tile)
import Experiments.WFC.Common.Tile as Tile


rotate : Tile -> Tile
rotate tile =
    Tile.rotate rotateView tile


rotateView : (Textures -> Canvas.Renderable) -> (Textures -> Canvas.Renderable)
rotateView texturesToRenderable =
    \textures ->
        let
            ( translateX, translateY ) =
                textures.blank
                    |> Canvas.dimensions
                    |> (\{ width, height } -> ( width / 2, height / 2 ))
        in
        Canvas.group
            [ Canvas.transform [ Canvas.translate translateX translateY, Canvas.rotate (degrees 90), Canvas.translate -translateX -translateY ] ]
            [ texturesToRenderable textures ]
