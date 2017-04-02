import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Subscriptions exposing (..)

import Html 
import Task
import Window


-- MAIN
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init: (Model, Cmd Msg)
init =
    (model, Task.perform MyWindowSize Window.size)

        
