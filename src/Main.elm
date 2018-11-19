port module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import Palette.X11 as X
import Panel
import PanelSample


port nextFrame : ({ width : Int, height : Int } -> msg) -> Sub msg


type Model
    = Model { width : Int, height : Int }


type Msg
    = Msg { width : Int, height : Int }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model { width = 0, height = 0 }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg rec ->
            ( Model rec
            , Cmd.none
            )


view : Model -> Html Msg
view (Model { width, height }) =
    Panel.toSvg
        { width = width, height = height }
        (PanelSample.depthList
            [ Panel.growGrowFromFixFix
                Panel.left
                Panel.top
                (PanelSample.horizontalListFixFix
                    [ Panel.fixGrowFromFixFix
                        Panel.centerY
                        (PanelSample.text { width = 32, height = 12 } "12345")
                    , Panel.fixGrowFromFixFix
                        Panel.centerY
                        (PanelSample.text { width = 32, height = 12 } "+")
                    , Panel.fixGrowFromFixFix
                        Panel.centerY
                        (PanelSample.text { width = 32, height = 12 } "abc")
                    ]
                )
            ]
        )


smallImage : Panel.FixFix msg
smallImage =
    PanelSample.imageCover
        { width = 32, height = 24 }
        imgSource


bigImage : Panel.FixFix msg
bigImage =
    PanelSample.imageCover
        { width = 96, height = 72 }
        imgSource


imgSource : String
imgSource =
    "image/img.jpg"


subscriptions : Model -> Sub Msg
subscriptions _ =
    nextFrame Msg
