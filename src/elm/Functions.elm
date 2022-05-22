module Functions exposing (..)

flip : (a -> b -> c) -> b -> a -> c
flip fun a b = fun b a

positionToSquareColored : (Int, Int) -> Int
positionToSquareColored (x, y) = x * 40 + y

squareColoredToPosition : Int -> (Int, Int)
squareColoredToPosition n = (n // 40, modBy n 40)

inits : List a -> List a
inits items =
    case items of
        [] -> []
        nonEmptyList ->
            nonEmptyList
                |> List.reverse
                |> List.tail
                |> Maybe.map List.reverse
                |> Maybe.withDefault []
