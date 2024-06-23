module Main exposing (..)
import Browser
import GraphicSVG.App exposing (graphicsApp)
import GraphicSVG as Svg
import Html exposing (Html)
import GraphicSVG.Widget as Widget
import Html exposing (div)
import Html exposing (img)
import Html.Attributes as HA
import Html.Events as HE
import Task
import Time as Time exposing (Posix, millisToPosix)
import Random
import List.Extra as LE
import GraphicSVG.App exposing (KeyState(..))

type alias SvgModel =
    { width : Float
    , height : Float
    }

type alias Model = 
    { svgModel : SvgModel 
    , dimensions : Int 
    , entropy : Int
    , tiles : List Tile
    , previousTiles : Maybe (List Tile)
    } 


type Tile 
    = Collapsed TileKind
    | Open (List TileKind)


type TileKind
    = Blank
    | DownT
    | LeftT
    | RightT
    | UpT

type Msg 
    = NoOp
    | GotNewEntropy Int
    | Redraw
    | Tick Posix
    | GotWidgetMsg Widget.Msg
    | RefreshEntropy

main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> (Model, Cmd Msg) 
init flags = 
    let
        dimensions = 3 

        width = 600.0
        
        height = 600.0

        (widget, widgetCmd) = Widget.init width height "svg"
    in
    ( { widget = widget
    , width = width
    , height = height
    , dimensions = dimensions
    , tiles = initTiles dimensions 
    -- , tiles =  [ Collapsed UpT, Collapsed RightT, Collapsed DownT, Collapsed LeftT]
    , previousTiles = Nothing 
    , entropy = 0 
    }
    , Cmd.batch 
        [ Cmd.map GotWidgetMsg widgetCmd
        , Task.perform (\_ -> Tick (millisToPosix 0)) Time.now
        , Random.generate GotNewEntropy (Random.int 0 100)
        ]
    )

initTiles : Int -> List Tile
initTiles dimensions =
    List.range 0 (dimensions * dimensions - 1) 
        |> List.map (always <| Open tileKinds)

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none) 

        GotNewEntropy newEntropy ->
            ( { model | entropy = newEntropy }
            , Cmd.none
            )
            
        Redraw ->
            ( { model | tiles = initTiles model.dimensions }
            , Cmd.none
            )

        Tick newTime ->
            ( collapse model newTime 
            , Random.generate GotNewEntropy (Random.int 0 100)
            )

        GotWidgetMsg widgetMsg ->
            let
                (newWidget, widgetCmd) = Widget.update widgetMsg model.widget
            in
            ( { model | widget = newWidget }
            , Cmd.map GotWidgetMsg widgetCmd
            )
        
        RefreshEntropy ->
            (model, Random.generate GotNewEntropy (Random.int 0 100))


collapse : Model -> Posix -> Model
collapse model newTime =
    let
        lowestEntropy =
            model.tiles
                |> List.filterMap
                    (\tile ->
                        case tile of
                            Collapsed _ ->
                                Nothing

                            Open kinds ->
                                Just <| List.length kinds 
                    )
                |> List.minimum
                |> Maybe.withDefault 0
                -- |> Debug.log "lowestEntropy"

        lowestEntropyTileIndex : Maybe Int
        lowestEntropyTileIndex =
            model.tiles
                |> List.indexedMap Tuple.pair 
                |> List.filter
                    (\(_, tile)->
                        case tile of
                            Collapsed _ ->
                                False

                            Open kinds ->
                                List.length kinds == lowestEntropy
                    )
                -- |> Debug.log "lowestEntropyTiles"
                |> getFromProbability model.entropy
                |> Maybe.map Tuple.first
                -- |> Debug.log "pickedIndex"

        newTiles =
            lowestEntropyTileIndex 
                |> Maybe.map (collapseTile model.entropy model.tiles) 
                -- |> Debug.log "post collapse"
                |> Maybe.map (cascadeCollapse model.dimensions)
                -- |> Debug.log "post cascade"

    in
    { model | tiles = newTiles |> Maybe.withDefault model.tiles }

collapseTile : Int -> List Tile -> Int -> List Tile
collapseTile entropy tiles index =
    tiles
        |> LE.updateAt index 
            (\tile ->
                case tile of 
                    Open kinds ->
                        Collapsed (LE.getAt (indexFromProbability entropy kinds) kinds |> Maybe.withDefault Blank)

                    Collapsed _ ->
                        tile
            )

cascadeCollapse : Int -> List Tile -> List Tile 
cascadeCollapse dimensions tiles =
    let 
        cascader index tile =
            case tile of 
                Collapsed _ ->
                    tile

                Open kinds ->
                    case collapser dimensions tiles kinds index of
                        [ kind ] ->
                            Collapsed kind

                        list ->
                            Open list
    in
    tiles
        |> List.indexedMap cascader

collapser : Int -> List Tile -> List TileKind -> Int -> List TileKind 
collapser dimensions tiles kinds index =
    let
        rightmostIndices = 
            lastIndicesOfRow dimensions 
            |> Debug.log "rightmostIndices"

        leftmostIndices = 
            firstIndicesOfRow dimensions 
            |> Debug.log "leftmostIndices"

        leftIndexValid = LE.notMember index leftmostIndices |> Debug.log "leftIndexValid"

        rightIndexValid = LE.notMember index rightmostIndices |> Debug.log "rightIndexValid"
    in
    lookUp (LE.getAt aboveIndex tiles) kinds 
    -- look right
        |> lookRight 
            ( if rightIndexValid then 
                LE.getAt rightIndex tiles
            else
                Nothing
            ) 
    -- look down
        |> lookDown (LE.getAt belowIndex tiles)
    -- look left
        |> lookLeft ( if leftIndexValid then 
                LE.getAt leftIndex tiles
            else
                Nothing
            ) 




lookUp : Maybe Tile -> List TileKind -> List TileKind 
lookUp maybeUpTile kinds =
    case maybeUpTile of 
        Just up ->
            case up of 
                Collapsed kind ->
                    case kind of 
                        Blank ->
                            kinds |> List.filter (\k -> List.member k [ DownT, Blank ])

                        DownT ->
                            kinds |> List.filter (\k -> List.member k[ DownT, LeftT, RightT, UpT ]) 

                        LeftT ->
                            kinds |> List.filter (\k -> List.member k[ LeftT, RightT, UpT ] )
 

                        RightT ->
                            kinds |> List.filter (\k -> List.member k[ LeftT, RightT, UpT ] )
 

                        UpT ->
                            kinds |> List.filter (\k -> List.member k[ DownT, Blank ]) 
 

                Open _ ->
                    kinds 

        Nothing ->
           kinds 

lookRight : Maybe Tile -> List TileKind -> List TileKind 
lookRight maybeRightTile kinds =
    case maybeRightTile of 
        Just right ->
            case right of 
                Collapsed kind ->
                    case kind of 
                        Blank ->
                            kinds |> List.filter (\k -> List.member k [ LeftT, Blank ])

                        DownT ->
                            kinds |> List.filter (\k -> List.member k [ DownT, RightT, UpT ]) 

                        LeftT ->
                            kinds |> List.filter (\k -> List.member k [ DownT, RightT, UpT ] )
 

                        RightT ->
                            kinds |> List.filter (\k -> List.member k [ LeftT, Blank] )
 

                        UpT ->
                            kinds |> List.filter (\k -> List.member k [ DownT, RightT, UpT ]) 
 

                Open _ ->
                    kinds 

        Nothing ->
           kinds 

lookDown : Maybe Tile -> List TileKind -> List TileKind
lookDown maybeDownTile kinds =
    case maybeDownTile of 
        Just down ->
            case down of 
                Collapsed kind ->
                    case kind of 
                        Blank ->
                            kinds |> List.filter (\k -> List.member k [ UpT, Blank ])

                        DownT ->
                            kinds |> List.filter (\k -> List.member k [ UpT, Blank ]) 

                        LeftT ->
                            kinds |> List.filter (\k -> List.member k [ LeftT, RightT, DownT] )
 

                        RightT ->
                            kinds |> List.filter (\k -> List.member k [ LeftT, RightT, DownT] )
 

                        UpT ->
                            kinds |> List.filter (\k -> List.member k [ LeftT, RightT, DownT ]) 
 

                Open _ ->
                    kinds 

        Nothing ->
           kinds

lookLeft : Maybe Tile -> List TileKind -> List TileKind
lookLeft maybeLeftTile kinds =
    case maybeLeftTile of 
        Just left ->
            case left of 
                Collapsed kind ->
                    case kind of 
                        Blank ->
                            kinds |> List.filter (\k -> List.member k [ Blank, RightT])

                        DownT ->
                            kinds |> List.filter (\k -> List.member k [ DownT, LeftT, UpT ]) 

                        LeftT ->
                            kinds |> List.filter (\k -> List.member k [ Blank, RightT] )
 

                        RightT ->
                            kinds |> List.filter (\k -> List.member k [ DownT, LeftT, UpT ] )
 

                        UpT ->
                            kinds |> List.filter (\k -> List.member k [ DownT, LeftT, UpT ]) 
 

                Open _ ->
                    kinds 

        Nothing ->
           kinds

view : Model -> Html Msg 
view model =
    div 
        [] 
        [ Widget.view model.widget
            [ List.indexedMap (viewTile model) model.tiles
                |> Svg.group
                |> Svg.move (-model.width/2, model.width/2) 

            ]
        , div 
            [] 
            [ Html.button 
                [ HE.onClick Redraw
                , HA.disabled <| not <| areAllCollapsed model.tiles 
                ]
                [ Html.text "Refresh" ]
            ]
        ]


viewTile : Model -> Int -> Tile -> Svg.Shape Msg
viewTile model index tile =
    let
        image = getTile tile

        tileSize = 50.0

        scale = model.width / toFloat model.dimensions / tileSize

        finalSize = tileSize * scale

        (x, y) = getXY index model.dimensions |> Tuple.mapBoth toFloat toFloat

        indexXY = (index, x, y )
    in
        image
            |> Svg.scale (model.width / toFloat model.dimensions / tileSize) 
            |> Svg.move  (finalSize * x, -finalSize * y)
 
    

getTile : Tile -> Svg.Shape Msg
getTile tile =
    case tile of 
        Collapsed kind ->
            case kind of 
                Blank ->
                    blankTile

                DownT ->
                    downTile

                LeftT ->
                    leftTile

                RightT ->
                    rightTile

                UpT ->
                    upTile

        Open kinds ->
            openTile 


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotWidgetMsg Widget.subscriptions 
        , Time.every 1000 Tick
        ]


tileKinds : List TileKind
tileKinds =
    [ Blank, DownT, LeftT, RightT, UpT ]

upTile : Svg.Shape Msg
upTile = 
    Svg.html 50.0 50.0 <| img [ HA.src "/up.png" ] []

downTile : Svg.Shape Msg
downTile = 
    Svg.html 50.0 50.0 <| img [ HA.src "/down.png" ] []

leftTile : Svg.Shape Msg
leftTile = 
    Svg.html 50.0 50.0 <| img [ HA.src "/left.png" ] []

rightTile : Svg.Shape Msg
rightTile = 
    Svg.html 50.0 50.0 <| img [ HA.src "/right.png" ] []

blankTile : Svg.Shape Msg
blankTile = 
    Svg.html 50.0 50.0 <| img [ HA.src "/blank.png" ] []

openTile : Svg.Shape Msg
openTile = 
    Svg.rect 50.0 50.0
        |> Svg.filled Svg.black

indexFromProbability : Int -> List a -> Int
indexFromProbability number list =
    let
        len = List.length list
        step = 100 // len
    in
    number // step


getFromProbability : Int -> List a -> Maybe a
getFromProbability number list =
    list |> LE.getAt (indexFromProbability number list)

  
lastIndicesOfRow : Int -> List Int
lastIndicesOfRow dim =
    List.range 1 (dim * dim)
        |> List.filter (\index -> remainderBy dim index == 0)
        |> List.map (\index -> index - 1)


firstIndicesOfRow : Int -> List Int
firstIndicesOfRow dim =
    lastIndicesOfRow dim
        |> List.map (\index -> index - dim + 1)


getXY : Int -> Int -> (Int, Int)
getXY index dim =
    let
        x = remainderBy dim index
        y = index // dim
    in
    (x, y)

areAllCollapsed : List Tile -> Bool
areAllCollapsed tiles =
    (tiles
        |> List.filter
            (\tile ->
                case tile of
                    Collapsed _ ->
                        False 

                    Open _ ->
                        True 
            )
        |> List.length) == 0