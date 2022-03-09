module MessageHelpers exposing (..)

import Task

sendCommandMessage : msg -> Cmd msg
sendCommandMessage msg =
    Task.perform (always msg) ( Task.succeed() )