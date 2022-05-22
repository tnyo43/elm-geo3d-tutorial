module Main exposing (main)

import Angle
import Array
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cylinder3d
import Direction3d
import FormatNumber
import FormatNumber.Locales as Locales
import Html exposing (Html, div, h1, h2, h3, table, td, text, tr)
import Json.Decode
import Json.Encode
import Length
import Pixels
import Point3d
import Quaternion
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import TriangularMesh
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
                            0.005 * (x - px)

                        dy =
                            0.005 * (y - py)
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


type WorldCoordinates
    = WorldCoordinates


blockMesh : Mesh.Uniform WorldCoordinates
blockMesh =
    let
        frontLeftTop =
            Point3d.meters 1 1 1

        frontRightTop =
            Point3d.meters 1 -1 1

        backLeftTop =
            Point3d.meters -1 1 1

        backRightTop =
            Point3d.meters -1 -1 1

        frontLeftBottom =
            Point3d.meters 1 1 -1

        frontRightBottom =
            Point3d.meters 1 -1 -1

        backLeftBottom =
            Point3d.meters -1 1 -1

        backRightBottom =
            Point3d.meters -1 -1 -1

        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ frontLeftTop -- 0
                    , frontRightTop -- 1
                    , backLeftTop -- 2
                    , backRightTop -- 3
                    , frontLeftBottom -- 4
                    , frontRightBottom -- 5
                    , backLeftBottom -- 6
                    , backRightBottom -- 7
                    ]
                )
                [ ( 0, 1, 4 ) -- front
                , ( 1, 4, 5 )
                , ( 0, 2, 4 ) -- left
                , ( 2, 4, 6 )
                , ( 2, 3, 6 ) -- back
                , ( 3, 6, 7 )
                , ( 1, 3, 5 ) -- right
                , ( 3, 5, 7 )
                , ( 0, 1, 2 ) -- top
                , ( 1, 2, 3 )
                , ( 4, 5, 6 ) -- bottom
                , ( 5, 6, 7 )
                ]
    in
    Mesh.indexedFacets triangularMesh


formatFloat : Float -> String
formatFloat =
    let
        locale =
            Locales.Locale
                (Locales.Exact 3)
                Locales.Western
                ","
                "."
                "-"
                ""
                "+"
                ""
                "±"
                ""
    in
    FormatNumber.format locale


viewQuaternion : Quaternion.Quaternion -> Html Msg
viewQuaternion q =
    let
        w =
            Quaternion.getW q

        x =
            Quaternion.getX q

        y =
            Quaternion.getY q

        z =
            Quaternion.getZ q
    in
    div [] [ formatFloat w ++ formatFloat x ++ "i" ++ formatFloat y ++ "j" ++ formatFloat z ++ "k" |> text ]


viewEulerAngles : ( Float, Float, Float ) -> Html Msg
viewEulerAngles ( roll, pitch, yaw ) =
    [ ( "roll", "X", roll ), ( "pitch", "Y", pitch ), ( "yaw", "Z", yaw ) ]
        |> List.map
            (\( key, axis, value ) ->
                tr []
                    [ td [] [ key ++ " (about " ++ axis ++ "-axis rotation)" |> text ]
                    , td [] [ text (value / Basics.pi * 180 |> formatFloat) ]
                    ]
            )
        |> table []


view : Model -> Html Msg
view model =
    let
        ( roll, pitch, yaw ) =
            toEulerRotation model.rotation

        blockEntity =
            Scene3d.mesh (Material.matte Color.white) blockMesh

        cylinderInfo =
            { radius = Length.meters 0.1
            , length = Length.meters 2
            }

        axiesEntity =
            [ Scene3d.cylinder
                (Material.color Color.red)
                (Cylinder3d.startingAt
                    Point3d.origin
                    Direction3d.x
                    cylinderInfo
                )
            , Scene3d.cylinder
                (Material.color Color.green)
                (Cylinder3d.startingAt
                    Point3d.origin
                    Direction3d.y
                    cylinderInfo
                )
            , Scene3d.cylinder
                (Material.color Color.blue)
                (Cylinder3d.startingAt
                    Point3d.origin
                    Direction3d.z
                    cylinderInfo
                )
            ]
                |> Scene3d.group

        rotate entity =
            entity
                |> Scene3d.rotateAround Axis3d.x (Angle.radians roll)
                |> Scene3d.rotateAround Axis3d.y (Angle.radians pitch)
                |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 9 0 0
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    div []
        [ h1 []
            [ text "Drag The Mouse To Rotate The Cube" ]
        , div []
            [ Scene3d.sunny
                { entities = [ blockEntity, axiesEntity ] |> Scene3d.group |> rotate |> List.singleton
                , camera = camera
                , upDirection = Direction3d.z
                , sunlightDirection = Direction3d.yz (Angle.degrees -120)
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 1
                , shadows = False
                , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                }
            , "* The cylinders represent the relative coordinate system of the cube (red: x, green: y, blue: z)" |> text
            ]
        , div []
            [ h2 []
                [ text "Rotation Parameters" ]
            , h3 [] [ text "In Quaternion Representation" ]
            , viewQuaternion model.rotation
            , h3 [] [ text "In Euler's Angles Representation (unit: degree)" ]
            , viewEulerAngles ( roll, pitch, yaw )
            ]
        ]
