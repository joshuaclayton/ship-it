module Data.Currency
    exposing
        ( Currency(..)
        , add
        , map
        , multiply
        , subtract
        , sum
        , zero
        )


type Currency
    = Currency Float


map : (Float -> Float) -> Currency -> Currency
map f (Currency c) =
    Currency <| f c


map2 : (Float -> Float -> Float) -> Currency -> Currency -> Currency
map2 f (Currency c1) (Currency c2) =
    Currency <| f c1 c2


add : Currency -> Currency -> Currency
add =
    map2 (+)


subtract : Currency -> Currency -> Currency
subtract =
    map2 (flip (-))


multiply : Currency -> Float -> Currency
multiply currency f =
    map ((*) f) currency


zero : Currency
zero =
    Currency 0


sum : List Currency -> Currency
sum =
    List.foldl add zero
