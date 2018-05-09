module View
    exposing
        ( view
        )

import Html exposing (..)
import Model exposing (Model)


view : Model -> Html a
view model =
    div []
        [ text "Hello world"
        , text <| toString <| model.toastMessages
        ]
