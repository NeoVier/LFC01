module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Automata as Automata
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { afds : List Automata.AFD
    , newAFD : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Nothing, Cmd.none )



-- UPDATE


type Msg
    = AFDRequested
    | AFDSelected File
    | AFDLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AFDRequested ->
            ( model, Select.file [ "text/txt" ] AFDSelected )

        AFDSelected file ->
            ( model, Task.perform AFDLoaded (File.toString file) )

        AFDLoaded content ->
            ( { model | newAFD = Just content }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.newAFD of
        Nothing ->
            button [ onClick AFDRequested ] [ text "Carregar AFD" ]

        Just content ->
            p [ style "white-space" "pre" ] [ text content ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
