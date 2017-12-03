module Main exposing (..)

import Messages exposing (Msg)
import Model exposing (Model)
import RouteUrl exposing (RouteUrlProgram)
import Update
import View


main : RouteUrlProgram Never Model Msg
main =
    RouteUrl.program
        { delta2url = Update.delta2url
        , location2messages = Update.url2messages
        , init = Update.init
        , update = Update.update
        , view = View.view
        , subscriptions = Update.subscriptions
        }
