module WFC.Svg.Grid exposing (collapse)

import Canvas.Texture exposing (dimensions)
import List.Extra as LE
import WFC.Common.Tile as Tile exposing (Direction)
import WFC.Svg.Types exposing (GridTile(..), Tile)


collapse : Int -> Float -> List GridTile -> List GridTile
collapse dimensions time grid =
    tileToCollapseIndex time grid
        |> Maybe.map (collapseTile time grid)
        |> Maybe.andThen (cascadeCollapse dimensions)
        |> Maybe.andThen (cascadeCollapse dimensions)
        |> Maybe.withDefault grid


collapseTile : Float -> List GridTile -> Int -> List GridTile
collapseTile time grid index =
    let
        collapseInto tiles =
            LE.getAt (indexFromProbability time tiles) tiles
    in
    grid
        |> LE.updateAt index
            (\tile ->
                case tile of
                    Open tiles ->
                        case collapseInto tiles of
                            Just choice ->
                                Collapsed choice

                            Nothing ->
                                tile

                    Collapsed _ ->
                        tile
            )


cascadeCollapse : Int -> List GridTile -> Maybe (List GridTile)
cascadeCollapse dimensions grid =
    let
        cascader : Int -> GridTile -> Maybe GridTile
        cascader index tile =
            case tile of
                Collapsed _ ->
                    Just tile

                Open tiles ->
                    case collapser dimensions grid tiles index of
                        [] ->
                            Nothing

                        [ tile_ ] ->
                            Just <| Collapsed tile_

                        list ->
                            Just <| Open list

        newGridOfMaybeTiles : List GridTile
        newGridOfMaybeTiles =
            grid
                |> List.indexedMap cascader
                |> List.filterMap identity
    in
    if List.length newGridOfMaybeTiles == List.length grid then
        Just newGridOfMaybeTiles

    else
        Nothing


collapser : Int -> List GridTile -> List Tile -> Int -> List Tile
collapser dimensions grid tiles index =
    tiles
        |> lookUp dimensions index grid
        |> lookRight dimensions index grid
        |> lookDown dimensions index grid
        |> lookLeft dimensions index grid


lookUp : Int -> Int -> List GridTile -> List Tile -> List Tile
lookUp dimensions index grid tiles =
    if index - dimensions >= 0 then
        lookAt Tile.Up (LE.getAt (index - dimensions) grid) tiles

    else
        tiles


lookRight : Int -> Int -> List GridTile -> List Tile -> List Tile
lookRight dimensions index grid tiles =
    if shouldLookRight dimensions index then
        lookAt Tile.Right (LE.getAt (index + 1) grid) tiles

    else
        tiles


lookDown : Int -> Int -> List GridTile -> List Tile -> List Tile
lookDown dimensions index grid tiles =
    if index + dimensions < List.length grid then
        lookAt Tile.Down (LE.getAt (index + dimensions) grid) tiles

    else
        tiles


lookLeft : Int -> Int -> List GridTile -> List Tile -> List Tile
lookLeft dimensions index grid tiles =
    if shouldLookLeft dimensions index then
        lookAt Tile.Left (LE.getAt (index - 1) grid) tiles

    else
        tiles


lookAt : Direction -> Maybe GridTile -> List Tile -> List Tile
lookAt direction maybeTile tiles =
    case maybeTile of
        Just gridTile ->
            case gridTile of
                Collapsed tile ->
                    tiles |> List.filter (Tile.filter direction tile)

                Open _ ->
                    tiles

        Nothing ->
            tiles


lowestEntropy : List GridTile -> Int
lowestEntropy grid =
    grid
        |> List.filterMap
            (\tile ->
                case tile of
                    Collapsed _ ->
                        Nothing

                    Open tiles ->
                        Just <| List.length tiles
            )
        |> List.minimum
        |> Maybe.withDefault 0


tileToCollapseIndex : Float -> List GridTile -> Maybe Int
tileToCollapseIndex time grid =
    let
        lowestEntropy_ =
            lowestEntropy grid
    in
    grid
        |> List.indexedMap Tuple.pair
        |> List.filter
            (\( _, tile ) ->
                case tile of
                    Collapsed _ ->
                        False

                    Open kinds ->
                        List.length kinds == lowestEntropy_
            )
        |> getFromProbability time
        |> Maybe.map Tuple.first


getFromProbability : Float -> List a -> Maybe a
getFromProbability time list =
    list |> LE.getAt (indexFromProbability time list)


indexFromProbability : Float -> List a -> Int
indexFromProbability time list =
    let
        randomProbability =
            probability time

        len =
            List.length list

        step =
            100 // len
    in
    randomProbability // step



{- Function to get a random number from 0 to 100 based on elapsed time -}


probability : Float -> Int
probability elapsedTime =
    let
        -- Get the decimal part of the elapsed time
        decimalPart =
            elapsedTime - toFloat (floor elapsedTime)

        -- Simple hash function to scramble the decimal part
        hash n =
            floor (100 * abs (sin (n * 1000)))

        -- Scale the decimal part to a value between 0 and 100
        scaledValue =
            modBy 101 (hash decimalPart)
    in
    scaledValue


lastIndicesOfRow : Int -> List Int
lastIndicesOfRow dim =
    List.range 1 (dim * dim)
        |> List.filter (\index -> remainderBy dim index == 0)
        |> List.map (\index -> index - 1)


firstIndicesOfRow : Int -> List Int
firstIndicesOfRow dim =
    lastIndicesOfRow dim
        |> List.map (\index -> index - dim + 1)


shouldLookRight : Int -> Int -> Bool
shouldLookRight dimensions index =
    lastIndicesOfRow dimensions
        |> LE.notMember index


shouldLookLeft : Int -> Int -> Bool
shouldLookLeft dimensions index =
    firstIndicesOfRow dimensions
        |> LE.notMember index
