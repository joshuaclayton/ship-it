module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Model exposing (Model, Msg(..))
import Data.Expirable as Expirable


init : ( Model, Cmd Msg )
init =
    Model.initial ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Expirable.expirableSubscription (always DecrementToastMessages)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        DecrementToastMessages ->
            { model | toastMessages = Expirable.tickAll model.toastMessages } ! []
