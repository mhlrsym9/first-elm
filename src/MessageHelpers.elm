module MessageHelpers exposing (..)

import Task

-- Useful function to send message up the window stack without dragging in the parent
-- to gain access to its update function

sendCommandMessage : msg -> Cmd msg
sendCommandMessage msg =
    Task.perform (always msg) ( Task.succeed() )