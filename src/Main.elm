module Main exposing (main)

import Angle
import Block3d
import Browser
import Camera3d
import Color
import Direction3d
import Html exposing (Html, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Length exposing (Length)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d


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
        ]
