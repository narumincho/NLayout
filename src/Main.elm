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
            [ PanelSample.colorGrowGrow X.brown
            , Panel.growGrowFromFitFit
                Panel.left
                Panel.bottom
                (PanelSample.colorFixFix { width = 60, height = 130 } X.skyBlue)
            , Panel.growGrowFromGrowFit
                Panel.top
                (PanelSample.colorGrowFix { height = 20 } X.skyBlue)
            , PanelSample.verticalListGrowGrow
                [ PanelSample.VerticalDivideItemFit
                    (Panel.growFitFromFitFit
                        Panel.left
                        smallImage
                    )
                , PanelSample.VerticalDivideItemFit
                    (Panel.growFitFromFitFit
                        Panel.centerX
                        smallImage
                    )
                , PanelSample.VerticalDivideItemFit
                    (Panel.growFitFromFitFit
                        Panel.right
                        smallImage
                    )
                , PanelSample.VerticalDivideItemFit
                    (PanelSample.colorGrowFix { height = 10 } X.darkBlue)
                , PanelSample.VerticalDivideItemGrow
                    1
                    (PanelSample.colorGrowGrow X.green)
                , PanelSample.VerticalDivideItemFit
                    (Panel.growFitFromFitFit
                        Panel.centerX
                        bigImage
                    )
                ]
            , Panel.growGrowFromFitGrow
                Panel.left
                (PanelSample.horizontalListFitGrow
                    [ Panel.fitGrowFromFitFit Panel.top bigImage
                    , Panel.fitGrowFromFitFit Panel.bottom smallImage
                    ]
                )
            , Panel.growGrowFromFitFit
                Panel.left
                Panel.centerY
                (PanelSample.text { width = 60, height = 130 } "Sample")
            ]
        )


smallImage : Panel.FitFit msg
smallImage =
    PanelSample.imageCover
        { width = 32, height = 24 }
        imgSource


bigImage : Panel.FitFit msg
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
