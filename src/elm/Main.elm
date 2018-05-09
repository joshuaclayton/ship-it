module Main exposing (main)

import Html
import Model
import Update
import View


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { subscriptions = Update.subscriptions
        , init = Update.init
        , view = View.view
        , update = Update.update
        }
