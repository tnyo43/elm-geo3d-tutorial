module Main exposing (main)

import Angle
import Block3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html, div, text)
import Json.Decode
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { cursor : Maybe ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing, Cmd.none )


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown position ->
            { model | cursor = Just position }

        MouseMove position ->
            { model
                | cursor =
                    if model.cursor == Nothing then
                        Nothing

                    else
                        Just position
            }

        MouseUp ->
            { model | cursor = Nothing }


type Msg
    = MouseDown ( Int, Int )
    | MouseMove ( Int, Int )
    | MouseUp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        ]


decodeMouse : (( Int, Int ) -> Msg) -> Json.Decode.Decoder Msg
decodeMouse toMsg =
    Json.Decode.map2 (\x y -> toMsg ( x, y ))
        (Json.Decode.field "pageX" Json.Decode.int)
        (Json.Decode.field "pageY" Json.Decode.int)


view : Model -> Html Msg
view model =
    div []
        [ Scene3d.unlit
            { dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
            , background = Scene3d.backgroundColor Color.grey
            , clipDepth = Length.meters 3.4
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { focalPoint = Point3d.origin
                            , eyePoint = Point3d.meters 9 0 0
                            , upDirection = Direction3d.positiveZ
                            }
                    , verticalFieldOfView = Angle.degrees 40
                    }
            , entities =
                [ Scene3d.block (Material.color Color.black)
                    (Block3d.with
                        { x1 = Length.meters 1
                        , x2 = Length.meters -1
                        , y1 = Length.meters 1
                        , y2 = Length.meters -1
                        , z1 = Length.meters 1
                        , z2 = Length.meters -1
                        }
                        |> Block3d.scaleAbout Point3d.origin (1 / 2)
                    )
                , Scene3d.sphere (Material.color Color.black)
                    (Sphere3d.atPoint
                        (Point3d.meters 1 1 1)
                        (Length.meters 0.3)
                    )
                ]
            }
        , case model.cursor of
            Nothing ->
                div [] []

            Just position ->
                text (position |> Tuple.first |> String.fromInt)
        ]
