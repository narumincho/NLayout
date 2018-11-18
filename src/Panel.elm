module Panel exposing
    ( FitFit
    , FitGrow
    , GrowFit
    , GrowGrow
    , HorizontalAlignment
    , VerticalAlignment
    , bottom
    , centerX
    , centerY
    , fitFitCustomize
    , fitFitGetHeight
    , fitFitGetWidth
    , fitFitToNSvgElementList
    , fitGrowCustomize
    , fitGrowFromFitFit
    , fitGrowGetMinHeight
    , fitGrowGetWidth
    , fitGrowToNSvgElementList
    , growFitCustomize
    , growFitFromFitFit
    , growFitGetHeight
    , growFitGetMinWidth
    , growFitToNSvgElementList
    , growGrowCustomize
    , growGrowFromFitFit
    , growGrowFromFitGrow
    , growGrowFromGrowFit
    , growGrowGetMinHeight
    , growGrowGetMinWidth
    , growGrowToNSvgElementList
    , left
    , right
    , toSvg
    , top
    )

import NSvg exposing (Svg)


{-| 幅と高さが中身によって決まるパネル
-}
type FitFit msg
    = FitFit
        { width : Int
        , height : Int
        , nSvgElementList : List (NSvg.Element msg)
        }


{-| 幅と高さが中身によって決まるパネルを作る
-}
fitFitCustomize : { width : Int, height : Int, nSvgElementList : List (NSvg.Element msg) } -> FitFit msg
fitFitCustomize =
    FitFit


{-| 幅は中身によって、高さは外の大きさによって決まるパネル
-}
type FitGrow msg
    = FitGrow
        { width : Int
        , minHeight : Int
        , nSvgElementList : { height : Int } -> List (NSvg.Element msg)
        }


{-| 幅は中身によって、高さは外の大きさによって決まるパネルを作る
-}
fitGrowCustomize :
    { width : Int, minHeight : Int, nSvgElementList : { height : Int } -> List (NSvg.Element msg) }
    -> FitGrow msg
fitGrowCustomize =
    FitGrow


{-| 縦方向の揃え方を指定してFitFitをFitGrowにする
-}
fitGrowFromFitFit : VerticalAlignment -> FitFit msg -> FitGrow msg
fitGrowFromFitFit verticalAlignment fitFit =
    FitGrow
        { width = fitFitGetWidth fitFit
        , minHeight = fitFitGetHeight fitFit
        , nSvgElementList =
            \{ height } ->
                fitFitToNSvgElementList fitFit
                    |> List.map
                        (NSvg.translate
                            { x = 0
                            , y =
                                verticalAlignmentToY
                                    { areaHeight = height
                                    , height = fitFitGetHeight fitFit
                                    }
                                    verticalAlignment
                            }
                        )
        }


{-| 幅は外の大きさによって、高さは中身によって決まるパネル
-}
type GrowFit msg
    = GrowFit
        { minWidth : Int
        , height : Int
        , nSvgElementList : { width : Int } -> List (NSvg.Element msg)
        }


{-| 幅は外の大きさによって、高さは中身によって決まるパネルをつくる
-}
growFitCustomize :
    { minWidth : Int, height : Int, nSvgElementList : { width : Int } -> List (NSvg.Element msg) }
    -> GrowFit msg
growFitCustomize =
    GrowFit


{-| 横方向の揃え方を指定してFiiFitをFitGrowにする
-}
growFitFromFitFit : HorizontalAlignment -> FitFit msg -> GrowFit msg
growFitFromFitFit horizontalAlignment fitFit =
    GrowFit
        { minWidth = fitFitGetWidth fitFit
        , height = fitFitGetHeight fitFit
        , nSvgElementList =
            \{ width } ->
                fitFitToNSvgElementList fitFit
                    |> List.map
                        (NSvg.translate
                            { x =
                                horizontalAlignmentToX
                                    { areaWidth = width
                                    , width = fitFitGetWidth fitFit
                                    }
                                    horizontalAlignment
                            , y = 0
                            }
                        )
        }


{-| 幅と高さが外の大きさによってきまるパネル
-}
type GrowGrow msg
    = GrowGrow
        { minWidth : Int
        , minHeight : Int
        , nSvgElementList : { width : Int, height : Int } -> List (NSvg.Element msg)
        }


{-| 幅と高さが外の大きさによってきまるパネルを作る
-}
growGrowCustomize :
    { minWidth : Int
    , minHeight : Int
    , nSvgElementList :
        { width : Int, height : Int }
        -> List (NSvg.Element msg)
    }
    -> GrowGrow msg
growGrowCustomize =
    GrowGrow


{-| 横方向の揃え方と縦方向の揃え方を指定してFitFitをGrowGrowにする
-}
growGrowFromFitFit : HorizontalAlignment -> VerticalAlignment -> FitFit msg -> GrowGrow msg
growGrowFromFitFit horizontalAlignment verticalAlignment fitFit =
    GrowGrow
        { minWidth = fitFitGetWidth fitFit
        , minHeight = fitFitGetHeight fitFit
        , nSvgElementList =
            \{ width, height } ->
                fitFitToNSvgElementList fitFit
                    |> List.map
                        (NSvg.translate
                            { x =
                                horizontalAlignmentToX
                                    { areaWidth = width
                                    , width = fitFitGetWidth fitFit
                                    }
                                    horizontalAlignment
                            , y =
                                verticalAlignmentToY
                                    { areaHeight = height
                                    , height = fitFitGetHeight fitFit
                                    }
                                    verticalAlignment
                            }
                        )
        }


{-| 横方向の揃え方を指定してFitGrowをGrowGrowにする
-}
growGrowFromFitGrow : HorizontalAlignment -> FitGrow msg -> GrowGrow msg
growGrowFromFitGrow horizontalAlignment fitGrow =
    GrowGrow
        { minWidth = fitGrowGetWidth fitGrow
        , minHeight = fitGrowGetMinHeight fitGrow
        , nSvgElementList =
            \{ width, height } ->
                fitGrowToNSvgElementList fitGrow { height = height }
                    |> List.map
                        (NSvg.translate
                            { x =
                                horizontalAlignmentToX
                                    { areaWidth = width
                                    , width = fitGrowGetWidth fitGrow
                                    }
                                    horizontalAlignment
                            , y = 0
                            }
                        )
        }


{-| 縦方向の揃え方を指定してGrowFitをGrowGrowにする
-}
growGrowFromGrowFit : VerticalAlignment -> GrowFit msg -> GrowGrow msg
growGrowFromGrowFit verticalAlignment growFit =
    GrowGrow
        { minWidth = growFitGetMinWidth growFit
        , minHeight = growFitGetHeight growFit
        , nSvgElementList =
            \{ width, height } ->
                growFitToNSvgElementList growFit { width = width }
                    |> List.map
                        (NSvg.translate
                            { x =
                                verticalAlignmentToY
                                    { areaHeight = height
                                    , height = growFitGetMinWidth growFit
                                    }
                                    verticalAlignment
                            , y = 0
                            }
                        )
        }


{-| 横方向のそろえ方
-}
type HorizontalAlignment
    = Left
    | CenterX
    | Right


{-| 横方向のそろえ方。左によせる
-}
left : HorizontalAlignment
left =
    Left


{-| 横方向のそろえ方。中央にそろえる
-}
centerX : HorizontalAlignment
centerX =
    CenterX


{-| 横方向のそろえ方。右によせる
-}
right : HorizontalAlignment
right =
    Right


{-| 縦のそろえ方
-}
type VerticalAlignment
    = Top
    | CenterY
    | Bottom


{-| 縦方向のそろえ方。上によせる
-}
top : VerticalAlignment
top =
    Top


{-| 縦方向のそろえ方。中央にそろえる
-}
centerY : VerticalAlignment
centerY =
    CenterY


{-| 縦方向のそろえ方。下によせる
-}
bottom : VerticalAlignment
bottom =
    Bottom


{-| 表示領域と表示幅と水平の揃え方からX座標を求める
-}
horizontalAlignmentToX : { areaWidth : Int, width : Int } -> HorizontalAlignment -> Int
horizontalAlignmentToX { areaWidth, width } horizontalAlignment =
    case horizontalAlignment of
        Left ->
            0

        CenterX ->
            (areaWidth - width) // 2

        Right ->
            areaWidth - width


{-| 表示領域と表示高さと垂直の揃え方からY座標を求める
-}
verticalAlignmentToY : { areaHeight : Int, height : Int } -> VerticalAlignment -> Int
verticalAlignmentToY { areaHeight, height } verticalAlignment =
    case verticalAlignment of
        Top ->
            0

        CenterY ->
            (areaHeight - height) // 2

        Bottom ->
            areaHeight - height


{-| 幅と高さとElementを渡してSvgにする
-}
toSvg : { width : Int, height : Int } -> GrowGrow msg -> Svg msg
toSvg { width, height } element =
    NSvg.toSvg
        { x = 0, y = 0, width = width, height = height }
        (growGrowToNSvgElementList element { width = width, height = height })


{-| GrowGrowを受け取り、NSvgElementのListを返す
-}
growGrowToNSvgElementList : GrowGrow msg -> { width : Int, height : Int } -> List (NSvg.Element msg)
growGrowToNSvgElementList (GrowGrow { nSvgElementList }) =
    nSvgElementList


{-| FitGrowと高さを受け取り、NSvgElementのListを返す
-}
fitGrowToNSvgElementList : FitGrow msg -> { height : Int } -> List (NSvg.Element msg)
fitGrowToNSvgElementList (FitGrow { nSvgElementList }) =
    nSvgElementList


{-| GrowFitと幅を受け取り、NSvgElementのListを返す
-}
growFitToNSvgElementList : GrowFit msg -> { width : Int } -> List (NSvg.Element msg)
growFitToNSvgElementList (GrowFit { nSvgElementList }) =
    nSvgElementList


{-| FitFitと幅と高さを受け取り、NSvgElementのListを返す
-}
fitFitToNSvgElementList : FitFit msg -> List (NSvg.Element msg)
fitFitToNSvgElementList (FitFit { nSvgElementList }) =
    nSvgElementList


{-| FitFitから幅を取得する
-}
fitFitGetWidth : FitFit msg -> Int
fitFitGetWidth (FitFit { width }) =
    width


{-| FitFitから高さを取得する
-}
fitFitGetHeight : FitFit msg -> Int
fitFitGetHeight (FitFit { height }) =
    height


{-| FitGrowから幅を取得する
-}
fitGrowGetWidth : FitGrow msg -> Int
fitGrowGetWidth (FitGrow { width }) =
    width


{-| FitGrowから最小の高さを取得する
-}
fitGrowGetMinHeight : FitGrow msg -> Int
fitGrowGetMinHeight (FitGrow { minHeight }) =
    minHeight


{-| GrowFitから高さを取得する
-}
growFitGetHeight : GrowFit msg -> Int
growFitGetHeight (GrowFit { height }) =
    height


{-| GrowFitから最小の幅を取得する
-}
growFitGetMinWidth : GrowFit msg -> Int
growFitGetMinWidth (GrowFit { minWidth }) =
    minWidth


{-| GrowGrowから最小の幅を取得する
-}
growGrowGetMinWidth : GrowGrow msg -> Int
growGrowGetMinWidth (GrowGrow { minWidth }) =
    minWidth


{-| GrowGrowから最小の高さを取得する
-}
growGrowGetMinHeight : GrowGrow msg -> Int
growGrowGetMinHeight (GrowGrow { minHeight }) =
    minHeight
