module Data.Expirable
    exposing
        ( Expirable
        , SecondsRemaining(SecondsRemaining)
        , expirableSubscription
        , expiresIn
        , tickAll
        , value
        )

import Time


type SecondsRemaining
    = SecondsRemaining Time.Time


type Expirable a
    = Expirable a SecondsRemaining


expiresIn : SecondsRemaining -> a -> Expirable a
expiresIn =
    flip Expirable


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
tick (Expirable a seconds) =
    let
        newSecondsRemaining =
            decrementSecondsRemaining seconds
    in
    if anySecondsRemaining newSecondsRemaining then
        Just <| Expirable a newSecondsRemaining
    else
        Nothing


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


value : Expirable a -> a
value (Expirable a _) =
    a
