module Data.Expirable
    exposing
        ( Expirable
        , SecondsRemaining(SecondsRemaining)
        , expirableSubscription
        , expiresIn
        , percentComplete
        , tickAll
        , value
        )

import Time


type SecondsRemaining
    = SecondsRemaining Time.Time


type SecondsTotal
    = SecondsTotal Time.Time


type Expirable a
    = Expirable a SecondsRemaining SecondsTotal


expiresIn : SecondsRemaining -> a -> Expirable a
expiresIn (SecondsRemaining total) value =
    Expirable value (SecondsRemaining total) (SecondsTotal total)


expirableSubscription : (Time.Time -> a) -> Sub a
expirableSubscription =
    Time.every Time.second


anySecondsRemaining : SecondsRemaining -> Bool
anySecondsRemaining (SecondsRemaining i) =
    i > 0


decrementSecondsRemaining : SecondsRemaining -> SecondsRemaining
decrementSecondsRemaining (SecondsRemaining i) =
    SecondsRemaining <| i - 1


tickAll : List (Expirable a) -> List (Expirable a)
tickAll =
    catMaybes << List.map tick


tick : Expirable a -> Maybe (Expirable a)
tick (Expirable a seconds secondsTotal) =
    let
        newSecondsRemaining =
            decrementSecondsRemaining seconds
    in
    if anySecondsRemaining newSecondsRemaining then
        Just <| Expirable a newSecondsRemaining secondsTotal
    else
        Nothing


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


value : Expirable a -> a
value (Expirable a _ _) =
    a


percentComplete : Expirable a -> Float
percentComplete (Expirable _ (SecondsRemaining remaining) (SecondsTotal total)) =
    remaining / total
