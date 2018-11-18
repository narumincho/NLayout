module NSvg exposing
    ( Element
    , Svg
    , fillColor
    , fillNone
    , imageContain
    , imageCover
    , rect
    , strokeColor
    , strokeColorWidth
    , strokeNone
    , toSvg
    , text
    , translate
    )

import Color exposing (Color)
import Svg as S
import Svg.Attributes as Sa



{-

   型のついたSVG要素に近い形式。
   要素を作ってから移動もできる

-}


type alias Svg msg =
    S.Svg msg


type Element msg
    = Rect { x : Int, y : Int, width : Int, height : Int, strokeStyle : StrokeStyle, fillStyle : FillStyle }
    | RectRound { x : Int, y : Int, width : Int, height : Int, rx : Int, ry : Int, strokeStyle : StrokeStyle, fillStyle : FillStyle }
    | ImageContain { x : Int, y : Int, width : Int, height : Int, sourceUrl : String }
    | ImageCover { x : Int, y : Int, width : Int, height : Int, sourceUrl : String }
    | Text { x : Int, y : Int, value : String, fontSize : Int }


toSvg : { x : Int, y : Int, width : Int, height : Int } -> List (Element msg) -> Svg msg
toSvg { x, y, width, height } children =
    S.svg
        [ Sa.viewBox
            (String.fromInt x
                ++ " "
                ++ String.fromInt y
                ++ " "
                ++ String.fromInt width
                ++ " "
                ++ String.fromInt height
            )
        ]
        (children |> List.map elementToSvg)


elementToSvg : Element msg -> Svg msg
elementToSvg nSvgElement =
    case nSvgElement of
        Rect { x, y, width, height, strokeStyle, fillStyle } ->
            S.rect
                ([ Sa.x (String.fromInt x)
                 , Sa.y (String.fromInt y)
                 , Sa.width (String.fromInt width)
                 , Sa.height (String.fromInt height)
                 ]
                    ++ strokeStyleToSvgAttributes strokeStyle
                    ++ fillStyleToSvgAttributes fillStyle
                )
                []

        RectRound { x, y, width, height, rx, ry, strokeStyle, fillStyle } ->
            S.rect
                ([ Sa.x (String.fromInt x)
                 , Sa.y (String.fromInt y)
                 , Sa.width (String.fromInt width)
                 , Sa.height (String.fromInt height)
                 , Sa.rx (String.fromInt rx)
                 , Sa.ry (String.fromInt ry)
                 ]
                    ++ strokeStyleToSvgAttributes strokeStyle
                    ++ fillStyleToSvgAttributes fillStyle
                )
                []

        ImageContain { x, y, width, height, sourceUrl } ->
            S.image
                [ Sa.x (String.fromInt x)
                , Sa.y (String.fromInt y)
                , Sa.width (String.fromInt width)
                , Sa.height (String.fromInt height)
                , Sa.xlinkHref sourceUrl
                , Sa.preserveAspectRatio "xMidYMid meet"
                ]
                []

        ImageCover { x, y, width, height, sourceUrl } ->
            S.image
                [ Sa.x (String.fromInt x)
                , Sa.y (String.fromInt y)
                , Sa.width (String.fromInt width)
                , Sa.height (String.fromInt height)
                , Sa.xlinkHref sourceUrl
                , Sa.preserveAspectRatio "xMidYMid slice"
                ]
                []

        Text { x, y, value, fontSize } ->
            S.text_
                [ Sa.x (String.fromInt x)
                , Sa.y (String.fromInt y)
                , Sa.fontSize (String.fromInt fontSize)
                ]
                [ S.text value ]


{-|

    線の表現

-}
type StrokeStyle
    = StrokeNone
    | StrokeWidthColor
        { color : Color
        }
    | StrokeWithColorWidth
        { color : Color
        , width : Int
        }


{-| 線を描かない
-}
strokeNone : StrokeStyle
strokeNone =
    StrokeNone


{-| 線の色だけ指定。線の太さは初期値の1
-}
strokeColor : Color -> StrokeStyle
strokeColor color =
    StrokeWidthColor
        { color = color }


{-| 線の色と幅を指定
-}
strokeColorWidth : Color -> Int -> StrokeStyle
strokeColorWidth color width =
    StrokeWithColorWidth
        { color = color
        , width = width
        }


{-| 線のスタイルをSvgの属性に変換
-}
strokeStyleToSvgAttributes : StrokeStyle -> List (S.Attribute msg)
strokeStyleToSvgAttributes strokeStyle =
    case strokeStyle of
        StrokeNone ->
            []

        StrokeWidthColor { color } ->
            [ Sa.stroke (Color.toRGBString color) ]

        StrokeWithColorWidth { color, width } ->
            [ Sa.stroke (Color.toRGBString color)
            , Sa.strokeWidth (String.fromInt width)
            ]


{-|

    塗りの表現

-}
type FillStyle
    = FillNone
    | FillWithColor { color : Color }


{-| 塗りつぶさない
-}
fillNone : FillStyle
fillNone =
    FillNone


{-| 単色で塗りつぶす
-}
fillColor : Color -> FillStyle
fillColor color =
    FillWithColor
        { color = color }


{-| 塗りつぶしのスタイルをSvgの属性に変換
-}
fillStyleToSvgAttributes : FillStyle -> List (S.Attribute msg)
fillStyleToSvgAttributes fillStyle =
    case fillStyle of
        FillNone ->
            []

        FillWithColor { color } ->
            [ Sa.fill (Color.toRGBString color) ]


rect : { width : Int, height : Int } -> StrokeStyle -> FillStyle -> Element msg
rect { width, height } strokeStyle fillStyle =
    Rect
        { x = 0, y = 0, width = width, height = height, strokeStyle = strokeStyle, fillStyle = fillStyle }


rectRound : { width : Int, height : Int, rx : Int, ry : Int } -> StrokeStyle -> FillStyle -> Element msg
rectRound { width, height, rx, ry } strokeStyle fillStyle =
    RectRound
        { x = 0, y = 0, width = width, height = height, rx = rx, ry = ry, strokeStyle = strokeStyle, fillStyle = fillStyle }


imageContain : { width : Int, height : Int, sourceUrl : String } -> Element msg
imageContain { width, height, sourceUrl } =
    ImageContain
        { x = 0, y = 0, width = width, height = height, sourceUrl = sourceUrl }


imageCover : { width : Int, height : Int, sourceUrl : String } -> Element msg
imageCover { width, height, sourceUrl } =
    ImageCover
        { x = 0, y = 0, width = width, height = height, sourceUrl = sourceUrl }


text : { fontSize : Int } -> String -> Element msg
text { fontSize } string =
    Text { x = 0, y = 0, value = string, fontSize = fontSize }


translate : { x : Int, y : Int } -> Element msg -> Element msg
translate { x, y } nSvgElement =
    case nSvgElement of
        Rect rec ->
            Rect
                { rec
                    | x = rec.x + x
                    , y = rec.y + y
                }

        RectRound rec ->
            RectRound
                { rec
                    | x = rec.x + x
                    , y = rec.y + y
                }

        ImageContain rec ->
            ImageContain
                { rec
                    | x = rec.x + x
                    , y = rec.y + y
                }

        ImageCover rec ->
            ImageCover
                { rec
                    | x = rec.x + x
                    , y = rec.y + y
                }

        Text rec ->
            Text
                { rec
                    | x = rec.x + x
                    , y = rec.y + y
                }
