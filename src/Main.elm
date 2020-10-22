module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Task
import Types.Types as Types
import View.View as View



-- MAIN


main : Program () Types.Model Types.Msg
main =
    Browser.element
        { init = Types.init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update msg model =
    case msg of
        Types.AFDRequested ->
            ( model, Select.file [ "text/txt" ] Types.AFDSelected )

        Types.AFDSelected file ->
            ( model, Task.perform Types.AFDLoaded (File.toString file) )

        -- TODO
        Types.AFDLoaded content ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none
