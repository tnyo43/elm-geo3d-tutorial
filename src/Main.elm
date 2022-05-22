module Main exposing (main)

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html, div, text)
import Json.Decode
import Json.Encode
import Length
import Pixels
import Point3d
import Quaternion
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
    { cursor : Maybe ( Float, Float )
    , rotation : Quaternion.Quaternion
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Quaternion.identity, Cmd.none )


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown position ->
            { model | cursor = Just position }

        MouseMove ( x, y ) ->
            case model.cursor of
                Nothing ->
                    model

                Just previousPosition ->
                    let
                        ( px, py ) =
                            previousPosition

                        dx =
                            0.01 * (x - px)

                        dy =
                            0.01 * (y - py)
                    in
                    { model
                        | cursor = Just ( x, y )
                        , rotation = model.rotation |> Quaternion.mul (Quaternion.zRotation dx) |> Quaternion.mul (Quaternion.yRotation dy)
                    }

        MouseUp ->
            { model | cursor = Nothing }


type Msg
    = MouseDown ( Float, Float )
    | MouseMove ( Float, Float )
    | MouseUp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        ]


decodeMouse : (( Float, Float ) -> Msg) -> Json.Decode.Decoder Msg
decodeMouse toMsg =
    Json.Decode.map2 (\x y -> toMsg ( x, y ))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


toEulerRotation : Quaternion.Quaternion -> ( Float, Float, Float )
toEulerRotation q =
    let
        w =
            Quaternion.getW q

        x =
            Quaternion.getX q

        y =
            Quaternion.getY q

        z =
            Quaternion.getZ q

        roll =
            Basics.atan2 (2 * (w * x + y * z)) (w * w - x * x - y * y + z * z)

        pitch =
            Basics.asin (2 * (w * y - x * z))

        yaw =
            Basics.atan2 (2 * (w * z + x * y)) (w * w + x * x - y * y - z * z)
    in
    ( roll, pitch, yaw )


view : Model -> Html Msg
view model =
    let
        ( roll, pitch, yaw ) =
            toEulerRotation model.rotation
    in
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
                , Scene3d.sphere (Material.color Color.red)
                    (Sphere3d.atPoint
                        (Point3d.meters 1 1 1)
                        (Length.meters 0.3)
                    )
                ]
                    |> Scene3d.group
                    |> Scene3d.rotateAround Axis3d.x (Angle.radians roll)
                    |> Scene3d.rotateAround Axis3d.y (Angle.radians pitch)
                    |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)
                    |> List.singleton
            }
        , case model.cursor of
            Nothing ->
                div [] []

            Just position ->
                text (position |> Tuple.first |> String.fromFloat)
        , div []
            [ model.rotation |> Quaternion.encode |> Json.Encode.encode 0 |> text ]
        , div []
            [ roll |> String.fromFloat |> text
            , pitch |> String.fromFloat |> text
            , yaw |> String.fromFloat |> text
            ]
        ]
