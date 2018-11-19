module Panel exposing
    ( FixFix, fixFixCustomize, fixFixGetWidth, fixFixGetHeight, fixFixToNSvgElementList
    , FixGrow, fixGrowCustomize, fixGrowFromFixFix, fixGrowGetWidth, fixGrowGetMinHeight, fixGrowToNSvgElementList
    , GrowFix, growFixCustomize, growFixFromFixFix, growFixGetMinWidth, growFixGetHeight, growFixToNSvgElementList
    , GrowGrow, growGrowCustomize, growGrowFromFixFix, growGrowFromFixGrow, growGrowFromGrowFix, growGrowGetMinWidth, growGrowGetMinHeight, growGrowToNSvgElementList
    , HorizontalAlignment, left, centerX, right
    , VerticalAlignment, top, centerY, bottom
    , horizontalAlignmentToX, toSvg, verticalAlignmentToY
    )

{-| レイアウトの基本。パネルが定義されている


# Fix Fix 幅と高さが固定サイズのパネル

@docs FixFix, fixFixCustomize, fixFixGetWidth, fixFixGetHeight, fixFixToNSvgElementList


# Fix Grow 幅が固定で、高さが外側に合わせて伸びるパネル

@docs FixGrow, fixGrowCustomize, fixGrowFromFixFix, fixGrowGetWidth, fixGrowGetMinHeight, fixGrowToNSvgElementList


# Grow Fix 幅が外側に合わせて伸びて、高さが固定のパネル

@docs GrowFix, growFixCustomize, growFixFromFixFix, growFixGetMinWidth, growFixGetHeight, growFixToNSvgElementList


# Grow Grow 幅と高さが外側に合わせて伸びるパネル

@docs GrowGrow, growGrowCustomize, growGrowFromFixFix, growGrowFromFixGrow, growGrowFromGrowFix, growGrowGetMinWidth, growGrowGetMinHeight, growGrowToNSvgElementList


# 横方向のそろえ方

@docs HorizontalAlignment, left, centerX, right


# 縦方向のそろえ方

@docs VerticalAlignment, top, centerY, bottom

-}

import NSvg


{-| 幅と高さが固定のパネル
-}
type FixFix msg
    = FixFix
        { width : Int
        , height : Int
        , nSvgElementList : List (NSvg.Element msg)
        }


{-| 幅と高さが中身によって決まるパネルを幅と高さと表示するSNvg.Elementから作る
-}
fixFixCustomize : { width : Int, height : Int, nSvgElementList : List (NSvg.Element msg) } -> FixFix msg
fixFixCustomize =
    FixFix


{-| 幅は中身によって、高さは外の大きさによって決まるパネル
-}
type FixGrow msg
    = FixGrow
        { width : Int
        , minHeight : Int
        , nSvgElementList : { height : Int } -> List (NSvg.Element msg)
        }


{-| 幅は中身によって、高さは外の大きさによって決まるパネルを作る
-}
fixGrowCustomize :
    { width : Int, minHeight : Int, nSvgElementList : { height : Int } -> List (NSvg.Element msg) }
    -> FixGrow msg
fixGrowCustomize =
    FixGrow


{-| 縦方向の揃え方を指定してFixFixをFixGrowにする
-}
fixGrowFromFixFix : VerticalAlignment -> FixFix msg -> FixGrow msg
fixGrowFromFixFix verticalAlignment fixFix =
    FixGrow
        { width = fixFixGetWidth fixFix
        , minHeight = fixFixGetHeight fixFix
        , nSvgElementList =
            \{ height } ->
                fixFixToNSvgElementList fixFix
                    |> List.map
                        (NSvg.translate
                            { x = 0
                            , y =
                                verticalAlignmentToY
                                    { areaHeight = height
                                    , height = fixFixGetHeight fixFix
                                    }
                                    verticalAlignment
                            }
                        )
        }


{-| 幅は外の大きさによって、高さは中身によって決まるパネル
-}
type GrowFix msg
    = GrowFix
        { minWidth : Int
        , height : Int
        , nSvgElementList : { width : Int } -> List (NSvg.Element msg)
        }


{-| 幅は外の大きさによって、高さは中身によって決まるパネルをつくる
-}
growFixCustomize :
    { minWidth : Int, height : Int, nSvgElementList : { width : Int } -> List (NSvg.Element msg) }
    -> GrowFix msg
growFixCustomize =
    GrowFix


{-| 横方向の揃え方を指定してFiiFixをFixGrowにする
-}
growFixFromFixFix : HorizontalAlignment -> FixFix msg -> GrowFix msg
growFixFromFixFix horizontalAlignment fixFix =
    GrowFix
        { minWidth = fixFixGetWidth fixFix
        , height = fixFixGetHeight fixFix
        , nSvgElementList =
            \{ width } ->
                fixFixToNSvgElementList fixFix
                    |> List.map
                        (NSvg.translate
                            { x =
                                horizontalAlignmentToX
                                    { areaWidth = width
                                    , width = fixFixGetWidth fixFix
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


{-| 横方向の揃え方と縦方向の揃え方を指定してFixFixをGrowGrowにする
-}
growGrowFromFixFix : HorizontalAlignment -> VerticalAlignment -> FixFix msg -> GrowGrow msg
growGrowFromFixFix horizontalAlignment verticalAlignment fixFix =
    GrowGrow
        { minWidth = fixFixGetWidth fixFix
        , minHeight = fixFixGetHeight fixFix
        , nSvgElementList =
            \{ width, height } ->
                fixFixToNSvgElementList fixFix
                    |> List.map
                        (NSvg.translate
                            { x =
                                horizontalAlignmentToX
                                    { areaWidth = width
                                    , width = fixFixGetWidth fixFix
                                    }
                                    horizontalAlignment
                            , y =
                                verticalAlignmentToY
                                    { areaHeight = height
                                    , height = fixFixGetHeight fixFix
                                    }
                                    verticalAlignment
                            }
                        )
        }


{-| 横方向の揃え方を指定してFixGrowをGrowGrowにする
-}
growGrowFromFixGrow : HorizontalAlignment -> FixGrow msg -> GrowGrow msg
growGrowFromFixGrow horizontalAlignment fixGrow =
    GrowGrow
        { minWidth = fixGrowGetWidth fixGrow
        , minHeight = fixGrowGetMinHeight fixGrow
        , nSvgElementList =
            \{ width, height } ->
                fixGrowToNSvgElementList fixGrow { height = height }
                    |> List.map
                        (NSvg.translate
                            { x =
                                horizontalAlignmentToX
                                    { areaWidth = width
                                    , width = fixGrowGetWidth fixGrow
                                    }
                                    horizontalAlignment
                            , y = 0
                            }
                        )
        }


{-| 縦方向の揃え方を指定してGrowFixをGrowGrowにする
-}
growGrowFromGrowFix : VerticalAlignment -> GrowFix msg -> GrowGrow msg
growGrowFromGrowFix verticalAlignment growFix =
    GrowGrow
        { minWidth = growFixGetMinWidth growFix
        , minHeight = growFixGetHeight growFix
        , nSvgElementList =
            \{ width, height } ->
                growFixToNSvgElementList growFix { width = width }
                    |> List.map
                        (NSvg.translate
                            { x =
                                verticalAlignmentToY
                                    { areaHeight = height
                                    , height = growFixGetMinWidth growFix
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
toSvg : { width : Int, height : Int } -> GrowGrow msg -> NSvg.Svg msg
toSvg { width, height } element =
    NSvg.toSvg
        { x = 0, y = 0, width = width, height = height }
        (growGrowToNSvgElementList element { width = width, height = height })


{-| GrowGrowを受け取り、NSvgElementのListを返す
-}
growGrowToNSvgElementList : GrowGrow msg -> { width : Int, height : Int } -> List (NSvg.Element msg)
growGrowToNSvgElementList (GrowGrow { nSvgElementList }) =
    nSvgElementList


{-| FixGrowと高さを受け取り、NSvgElementのListを返す
-}
fixGrowToNSvgElementList : FixGrow msg -> { height : Int } -> List (NSvg.Element msg)
fixGrowToNSvgElementList (FixGrow { nSvgElementList }) =
    nSvgElementList


{-| GrowFixと幅を受け取り、NSvgElementのListを返す
-}
growFixToNSvgElementList : GrowFix msg -> { width : Int } -> List (NSvg.Element msg)
growFixToNSvgElementList (GrowFix { nSvgElementList }) =
    nSvgElementList


{-| FixFixと幅と高さを受け取り、NSvgElementのListを返す
-}
fixFixToNSvgElementList : FixFix msg -> List (NSvg.Element msg)
fixFixToNSvgElementList (FixFix { nSvgElementList }) =
    nSvgElementList


{-| FixFixから幅を取得する
-}
fixFixGetWidth : FixFix msg -> Int
fixFixGetWidth (FixFix { width }) =
    width


{-| FixFixから高さを取得する
-}
fixFixGetHeight : FixFix msg -> Int
fixFixGetHeight (FixFix { height }) =
    height


{-| FixGrowから幅を取得する
-}
fixGrowGetWidth : FixGrow msg -> Int
fixGrowGetWidth (FixGrow { width }) =
    width


{-| FixGrowから最小の高さを取得する
-}
fixGrowGetMinHeight : FixGrow msg -> Int
fixGrowGetMinHeight (FixGrow { minHeight }) =
    minHeight


{-| GrowFixから高さを取得する
-}
growFixGetHeight : GrowFix msg -> Int
growFixGetHeight (GrowFix { height }) =
    height


{-| GrowFixから最小の幅を取得する
-}
growFixGetMinWidth : GrowFix msg -> Int
growFixGetMinWidth (GrowFix { minWidth }) =
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
