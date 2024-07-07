module Utils.Random exposing (..)

{-| Function to get a random number from 0 to 100
based on elapsed time
-}


probabilityFromTime : Float -> Int
probabilityFromTime elapsedTime =
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


randomBetween : Int -> Int -> Float -> Int
randomBetween min max elapsedTime =
    let
        scaledValue =
            probabilityFromTime elapsedTime

        range =
            max - min

        scaled =
            toFloat scaledValue / 100

        value =
            min + round (scaled * toFloat range)
    in
    value
