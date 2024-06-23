module Grid exposing (collapse)
import Types exposing (GridTile(..))
import List.Extra as LE
import Tile exposing (Tile)
import Types exposing (Msg)


collapse : Int -> Float -> List GridTile -> List GridTile
collapse dimensions time grid =
    let
        collapseAt =
            tileToCollapseIndex time grid

        temporaryCollapse =
            collapseAt
                |> Maybe.map (collapseTile time grid)
                -- |> Maybe.andThen (cascadeCollapse dimensions)

    in
    -- case temporaryCollapse of
    --     Just collapsed ->
    --         collapsed 

    --     Nothing ->
    --         grid


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


collapser : Int -> List GridTile -> List (Tile Int Msg) -> Int -> List (Tile Int Msg) 
collapser dimensions grid tiles index =
    let
        leftIndexValid =
            lastIndicesOfRow dimensions
                |> LE.notMember index

        rightIndexValid =
            firstIndicesOfRow dimensions
                |> LE.notMember index

        up =
            LE.getAt (index - dimensions) tiles

        right =
            if rightIndexValid then
                LE.getAt (index + 1) tiles
            else
                Nothing

        down =
            LE.getAt (index + dimensions) tiles

        left =
            if leftIndexValid then
                LE.getAt (index - 1) tiles
            else
                Nothing

    in
    tiles
        |> lookUp up 
        |> lookRight right
        |> lookDown down 
        |> lookLeft left 



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
            (\(_, tile)->
                case tile of
                    Collapsed _ ->
                        False

                    Open kinds ->
                        List.length kinds == lowestEntropy_
            )
        |> getFromProbability time
        |> Maybe.map Tuple.first


getFromProbability : Float -> List a -> Maybe a
getFromProbability  time list =
    list |> LE.getAt (indexFromProbability time list)

indexFromProbability : Float -> List a -> Int
indexFromProbability time list =
    let
        randomProbability = probability time
        len = List.length list
        step = 100 // len
    in
    randomProbability // step


{- Function to get a random number from 0 to 100 based on elapsed time
-}
probability : Float -> Int
probability elapsedTime =
    let
        -- Get the decimal part of the elapsed time
        decimalPart = elapsedTime - toFloat (floor elapsedTime)
        -- Scale the decimal part to a value between 0 and 100
        scaledValue = floor (decimalPart * 100)
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

