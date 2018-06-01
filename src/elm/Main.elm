module Main exposing (main)

import Html
import Model
import Persistence.Ports exposing (ports)
import Update
import View


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { subscriptions = Update.subscriptions
        , init = Update.init ports
        , view = View.view
        , update = Update.update
        }
