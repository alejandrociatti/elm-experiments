module Utils.Grid exposing (Grid, fill, generate, get, map, shiftDown)

import List.Extra as LE

type alias Grid a =
    { data : List a
    , rows : Int
    , cols : Int
    }

fill : Int -> Int -> a -> Grid a
fill rows cols value =
    { data = List.repeat (rows * cols) value
    , rows = rows
    , cols = cols
    }

generate : Int -> Int -> (Int -> Int -> a) -> Grid a
generate rows cols generator =
    let
        data = List.concatMap (\i -> List.map (generator i) (List.range 0 (cols - 1))) (List.range 0 (rows - 1))
    in
    { data = data, rows = rows, cols = cols }

get : Int -> Int -> Grid a -> Maybe a
get row col matrix =
    if row < 0 || col < 0 || row >= matrix.rows || col >= matrix.cols then
        Nothing
    else
        LE.getAt (row * matrix.cols + col) matrix.data
    

map : (a -> b) -> Grid a -> Grid b
map f grid =
    { data = List.map f grid.data 
    , rows = grid.rows
    , cols = grid.cols
    } 


shiftUp : Grid a -> Grid a
shiftUp grid =
    let
        (firstRow, restRows) = LE.splitAt grid.cols grid.data
    in
    { grid | data = restRows ++ firstRow }

shiftDown : Grid a -> Grid a
shiftDown grid =
    let
        (lastRow, restRows) = LE.splitAt (List.length grid.data - grid.cols) grid.data
    in
    { grid | data = lastRow ++ restRows }
