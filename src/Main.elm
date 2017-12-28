module Main exposing (..)

import Messages exposing (Msg)
import Model exposing (Flags, Model)
import RouteUrl exposing (RouteUrlProgram)
import Update
import View


main : RouteUrlProgram Flags Model Msg
main =
    RouteUrl.programWithFlags
        { delta2url = Update.delta2url
        , location2messages = Update.url2messages
        , init = Update.init
        , update = Update.update
        , view = View.view
        , subscriptions = Update.subscriptions
        }
