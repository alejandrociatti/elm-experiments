module Experiments.Landscape exposing (main)

import Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d
import TriangularMesh exposing (TriangularMesh)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Scene3d.Mesh exposing (indexedTriangles, indexedFacets, indexedFaces)
import Utils.Random exposing (probabilityFromTime)
import Utils.Number exposing (mapValues)
import Vector3d exposing (Vector3d)
import Utils.Grid as Grid exposing (Grid)
import Quantity exposing (Unitless)

type alias Model = 
    { heights : Grid Float
    }


main : Html msg
main =
    Scene3d.cloudy
        { dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        , upDirection = Direction3d.positiveZ
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters 500 500 0 
                        , eyePoint = Point3d.meters 500 200 200
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30 
                }
        , clipDepth = Length.meters 1 
        , background = Scene3d.transparentBackground
        , entities =
            [ Scene3d.mesh (Material.matte Color.blue)
                -- <| indexedTriangles <| grid drawGridPoint
                <| indexedFacets <| grid drawGridPoint
            ]
        }


meshSizeX : Float 
meshSizeX = 1000

meshSizeY : Float 
meshSizeY = 500


grid : (Float -> Float -> vertex) -> TriangularMesh vertex 
grid =
    TriangularMesh.grid 100 100


drawGridPoint : Float -> Float -> Point3d Meters coordinates
drawGridPoint x y =
    Point3d.meters (x*meshSizeX) (y*meshSizeY) (mapValues 0 100 -10 10 (toFloat <| probabilityFromTime (x*y))) 


drawGridPointWithNormal : Float -> Float ->  { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates } 
drawGridPointWithNormal x y =
    { position = drawGridPoint x y 
    , normal = Vector3d.unitless 0 0 1
    }