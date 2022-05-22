module Main exposing (main)

import Browser
import Html exposing (Html, text)


main : Program () Model msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { text : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model "hi", Cmd.none )


update : a -> Model -> Model
update _ model =
    model


subscriptions : a -> Sub msg
subscriptions _ =
    Sub.batch []


view : a -> Html msg
view _ =
    text "Hello Elm"
