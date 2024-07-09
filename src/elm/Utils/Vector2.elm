module Utils.Vector2 exposing (add, distance, fromInt, heading, isZero, normalize, toInt, magnitude, negate)


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    let
        magnitude_ =
            magnitude (x, y)
    in
      
    if magnitude_ == 0 then
        ( 0, 0 )
        -- Handle zero vector to avoid division by zero

    else
        ( x / magnitude_, y / magnitude_ )


magnitude : ( Float, Float ) -> Float
magnitude (x, y) =
            sqrt (x * x + y * y)


isZero : ( number, number ) -> Bool
isZero ( x, y ) =
    x == 0 && y == 0


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


heading : ( Float, Float ) -> Float
heading ( x, y ) =
    atan2 y x


fromInt : ( Int, Int ) -> ( Float, Float )
fromInt ( x, y ) =
    ( toFloat x, toFloat y )


toInt : ( Float, Float ) -> ( Int, Int )
toInt ( x, y ) =
    ( round x, round y )


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


negate : ( number, number ) -> ( number, number )
negate ( x, y ) =
    ( -x, -y )