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


type alias LastTicked =
    Maybe Time.Time


type Expirable a
    = Expirable a SecondsRemaining SecondsTotal LastTicked


expiresIn : SecondsRemaining -> a -> Expirable a
expiresIn (SecondsRemaining total) value =
    Expirable value (SecondsRemaining total) (SecondsTotal total) Nothing


expirableSubscription : (Time.Time -> a) -> Sub a
expirableSubscription =
    Time.every Time.second


anySecondsRemaining : SecondsRemaining -> Bool
anySecondsRemaining (SecondsRemaining i) =
    i > 0


tickAll : Time.Time -> List (Expirable a) -> List (Expirable a)
tickAll time =
    catMaybes << List.map (tick time)


decreaseSecondsRemaining : Float -> SecondsRemaining -> SecondsRemaining
decreaseSecondsRemaining i (SecondsRemaining remaining) =
    SecondsRemaining <| remaining - i


tick : Time.Time -> Expirable a -> Maybe (Expirable a)
tick time (Expirable a seconds secondsTotal lastTicked) =
    let
        newSecondsRemaining =
            decreaseSecondsRemaining secondToDecrease seconds

        secondToDecrease =
            toFloat <|
                case lastTicked of
                    Nothing ->
                        1

                    Just oldTime ->
                        round <| (time - oldTime) / 1000
    in
    if anySecondsRemaining newSecondsRemaining then
        Just <| Expirable a newSecondsRemaining secondsTotal (Just time)
    else
        Nothing


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


value : Expirable a -> a
value (Expirable a _ _ _) =
    a


percentComplete : Expirable a -> Float
percentComplete (Expirable _ (SecondsRemaining remaining) (SecondsTotal total) _) =
    remaining / total
