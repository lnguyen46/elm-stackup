module Subscriptions exposing (..)

import Update exposing (..)
import Model exposing (..)
import Window
import Keyboard
import AnimationFrame

-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model =
    case model.size.width of
        0 ->
            Window.resizes MyWindowSize
                
        otherwise ->
            case model.blockState of
                ToRight ->
                    Sub.batch
                        [ Keyboard.downs Keydown
                        , AnimationFrame.times GoRight
                        ]
                        
                ToLeft ->
                    Sub.batch
                        [ Keyboard.downs Keydown
                        , AnimationFrame.times GoLeft
                        ]

                Falling ->
                    AnimationFrame.times FreeFall
