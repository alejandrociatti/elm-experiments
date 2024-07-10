module Experiments.Landscape exposing (Model, Msg, init, subscriptions, update, view)

import Angle as Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length exposing (Meters)
import Pixels
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (indexedFacets)
import Simplex exposing (PermutationTable)
import TriangularMesh exposing (TriangularMesh)
import Utils.Grid as Grid exposing (Grid)
import Utils.Number exposing (mapValues)
import Utils.Random exposing (probabilityFromTime)
import Utils.Tick as Tick
import Vector3d as Vector3d exposing (Vector3d)
import Viewpoint3d



-- CONSTANTS


meshSizeX : Float
meshSizeX =
    1400


meshSizeY : Float
meshSizeY =
    1000


meshColumns : number
meshColumns =
    100


meshRows : number
meshRows =
    100



-- TODO: add controls
-- one control for FX/ fancy (reduces mesh complexiry but adds shadow version)
-- control speed / mesh size / noise settings !!
-- control camera !


type alias Model =
    { offset : Float
    , tick : Tick.Model
    }


type Msg
    = GotTickMsg Tick.Msg



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42



-- Create a function for 2D fractal noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 0.4, steps = 6, stepSize = 2.0, persistence = 2.0 } permTable


init : Model
init =
    { offset = 0
    , tick = Tick.init
    }


view : Model -> Html msg
view model =
    let
        objectMesh : Mesh.Uniform coordinates
        objectMesh =
            indexedFacets <|
                grid (drawGridPoint model.offset)
    in
    Scene3d.sunny
        { dimensions = ( Pixels.pixels 1080, Pixels.pixels 720 )
        , upDirection = Direction3d.positiveZ

        -- , sunlightDirection =  Direction3d.xyZ (Angle.degrees 45) (Angle.degrees 30)
        -- , sunlightDirection = sunsetBehind
        , sunlightDirection = sunsetLeft

        -- , sunlightDirection = sunsetIntoCam
        , shadows = True
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters (meshSizeX / 2) 500 0
                        , eyePoint = Point3d.meters (meshSizeX / 2) -350 380
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , entities =
            [ Scene3d.mesh (Material.metal { baseColor = Color.blue, roughness = 0.1})
                objectMesh
            ]
        }


viewFancy : Model -> Html msg
viewFancy model =
    let
        objectMesh : Mesh.Uniform coordinates
        objectMesh =
            indexedFacets <|
                grid (drawGridPoint model.offset)

        objectShadow =
            Mesh.shadow objectMesh
    in
    Scene3d.sunny
        { dimensions = ( Pixels.pixels 1080, Pixels.pixels 720 )
        , upDirection = Direction3d.positiveZ

        -- , sunlightDirection =  Direction3d.xyZ (Angle.degrees 45) (Angle.degrees 30)
        -- , sunlightDirection = sunsetBehind
        , sunlightDirection = sunsetLeft

        -- , sunlightDirection = sunsetIntoCam
        , shadows = True
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters (meshSizeX / 2) 500 0
                        , eyePoint = Point3d.meters (meshSizeX / 2) -350 380
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , entities =
            [ Scene3d.meshWithShadow (Material.matte Color.blue)
                objectMesh
                objectShadow
            ]
        }


grid : (Float -> Float -> vertex) -> TriangularMesh vertex
grid =
    TriangularMesh.grid meshColumns meshRows


drawGridPoint : Float -> Float -> Float -> Point3d Meters coordinates
drawGridPoint offset x y =
    let
        noise_ =
            noise (x * 5) (5 * y + offset)
                |> mapValues 0 1 -50 100
    in
    Point3d.meters
        (x * meshSizeX)
        (y * meshSizeY)
        noise_


sunsetIntoCam =
    Direction3d.xyZ (Angle.degrees 0) (Angle.degrees 0)


sunsetLeft =
    Direction3d.xyZ (Angle.degrees 90) (Angle.degrees 0)


sunsetBehind =
    Direction3d.xyZ (Angle.degrees 180) (Angle.degrees 0)


subscriptions : Sub Msg
subscriptions =
    Tick.subscription |> Sub.map GotTickMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotTickMsg tickMsg ->
            let
                newTick =
                    Tick.update tickMsg model.tick

                offset =
                    model.offset + 0.1
            in
            { model | tick = newTick, offset = offset }
