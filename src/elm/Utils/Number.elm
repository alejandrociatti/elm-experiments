module Utils.Number exposing (..)


constrain : number -> number -> number -> number
constrain min max value =
    if value < min then
        min

    else if value > max then
        max

    else
        value


constrainReverse : number -> number -> number -> number
constrainReverse min max value =
    if value > max then
        min

    else if value < min then
        max

    else
        value


mapValues : Float -> Float -> Float -> Float -> Float -> Float
mapValues min max newMin newMax value =
    let
        range =
            max - min

        newRange =
            newMax - newMin

        scaled =
            (value - min) / range
    in
    newMin + scaled * newRange
