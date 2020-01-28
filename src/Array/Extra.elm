module Array.Extra exposing (mapAtIndex, removeItem, removeItemIf)

import Array exposing (Array)


mapAtIndex : Int -> (a -> a) -> Array a -> Array a
mapAtIndex index fn xs =
    case Array.get index xs of
        Just x ->
            Array.set index (fn x) xs

        Nothing ->
            xs


removeItem : Int -> Array a -> Array a
removeItem index arr =
    Array.append (Array.slice 0 index arr) (Array.slice (index + 1) (Array.length arr) arr)


removeItemIf : (a -> Bool) -> Int -> Array a -> Array a
removeItemIf fn index xs =
    case Array.get index xs of
        Just x ->
            if fn x then
                removeItem index xs

            else
                xs

        Nothing ->
            xs
