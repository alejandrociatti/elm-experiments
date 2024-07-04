module Experiments.Raycast.Vector exposing (distance, normalize)


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    let
        magnitude =
            sqrt (x * x + y * y)
    in
    if magnitude == 0 then
        ( 0, 0 )
        -- Handle zero vector to avoid division by zero

    else
        ( x / magnitude, y / magnitude )


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
