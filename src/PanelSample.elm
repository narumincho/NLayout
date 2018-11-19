module PanelSample exposing
    ( colorGrowGrow, colorGrowFix, colorFixGrow, colorFixFix
    , imageContain, imageCover
    , text
    , boxFixFix
    , depthList
    , VerticalListItem(..), verticalListGrowGrow, verticalListGrowFix, verticalListFixGrow, verticalListFixFix
    , HorizontalListItem(..), horizontalListGrowGrow, horizontalListGrowFix, horizontalListFixGrow, horizontalListFixFix
    )

{-| よく使うPanelのサンプルを多数収録!

# 単色パネル

@docs colorGrowGrow, colorGrowFix, colorFixGrow, colorFixFix


# 画像パネル

@docs imageContain, imageCover


# テキストパネル

@docs text


# 固定サイズパネル

@docs boxFixFix


# 奥行きリストパネル

@docs depthList


# 縦並びリストパネル

@docs VerticalListItem, verticalListGrowGrow, verticalListGrowFix, verticalListFixGrow, verticalListFixFix


# 横並びリストパネル

@docs HorizontalListItem, horizontalListGrowGrow, horizontalListGrowFix, horizontalListFixGrow, horizontalListFixFix

-}

import Color exposing (Color)
import NSvg exposing (Svg)
import Palette.X11 as X
import Panel



{-
   =========================================
               固定サイズパネル
   =========================================
-}

{-|
    GrowGrowパネルを固定サイズにする
-}
boxFixFix : { width : Int, height : Int } -> Panel.GrowGrow msg -> Panel.FixFix msg
boxFixFix { width, height } growGrow =
    Panel.fixFixCustomize
        { width = width
        , height = height
        , nSvgElementList =
            Panel.growGrowToNSvgElementList growGrow { width = width, height = height }
        }



{-
   =========================================
                   単色パネル
   =========================================
-}


{-| 単色で伸びる要素
-}
colorGrowGrow : Color -> Panel.GrowGrow msg
colorGrowGrow color =
    Panel.growGrowCustomize
        { minWidth = 0
        , minHeight = 0
        , nSvgElementList =
            \{ width, height } ->
                [ NSvg.rect
                    { width = width, height = height }
                    NSvg.strokeNone
                    (NSvg.fillColor color)
                ]
        }


{-| 単色で高さが固定で横に伸びる
-}
colorGrowFix : { height : Int } -> Color -> Panel.GrowFix msg
colorGrowFix { height } color =
    Panel.growFixCustomize
        { minWidth = 0
        , height = height
        , nSvgElementList =
            \{ width } ->
                [ NSvg.rect
                    { width = width, height = height }
                    NSvg.strokeNone
                    (NSvg.fillColor color)
                ]
        }


{-| 単色で幅が固定で縦に伸びる
-}
colorFixGrow : { width : Int } -> Color -> Panel.FixGrow msg
colorFixGrow { width } color =
    Panel.fixGrowCustomize
        { width = width
        , minHeight = 0
        , nSvgElementList =
            \{ height } ->
                [ NSvg.rect
                    { width = width, height = height }
                    NSvg.strokeNone
                    (NSvg.fillColor color)
                ]
        }


{-| 単色で固定サイズの要素
-}
colorFixFix : { width : Int, height : Int } -> Color -> Panel.FixFix msg
colorFixFix { width, height } color =
    Panel.fixFixCustomize
        { width = width
        , height = height
        , nSvgElementList =
            [ NSvg.rect
                { width = width, height = height }
                NSvg.strokeNone
                (NSvg.fillColor color)
            ]
        }



{-
   =========================================
                   画像パネル
    本当は、幅、高さのどちらかを取得して画像データから残りの幅、高さを算出できるようする
    keepAspectImagePanelFromWidth : {width : Int} -> Image -> PanelFixFix msg
    keepAspectImagePanelFromHeight : {height: Int} -> Image -> PanelFixFix msg
    を作りたかった
    Elmでは無理なのでDefinyで挑戦したい
   =========================================
-}


{-|

    画像パネル
    画像を{width,height}で指定した比率に合うようにレターボックス(透明の帯)を上下、左右の足りない方に追加する

-}
imageContain : { width : Int, height : Int } -> String -> Panel.FixFix msg
imageContain { width, height } source =
    Panel.fixFixCustomize
        { width = width
        , height = height
        , nSvgElementList =
            [ NSvg.imageContain
                { width = width, height = height, sourceUrl = source }
            ]
        }


{-|

    画像パネル
    画像を{width,height}で指定した比率に合うように飛び出たところを消す

-}
imageCover : { width : Int, height : Int } -> String -> Panel.FixFix msg
imageCover { width, height } source =
    Panel.fixFixCustomize
        { width = width
        , height = height
        , nSvgElementList =
            [ NSvg.imageCover
                { width = width, height = height, sourceUrl = source }
            ]
        }



{-
   =========================================
                   テキスト
   =========================================
-}


text : { width : Int, height : Int } -> String -> Panel.FixFix msg
text { width, height } string =
    Panel.fixFixCustomize
        { width = width
        , height = height
        , nSvgElementList =
            [ NSvg.text { fontSize = 12 } string
            ]
        }



{-
   =========================================
                   縦並びリスト
   =========================================
-}


{-| 縦並び分割、幅と高さは中身に合わせる
-}
verticalListFixFix : List (Panel.GrowFix msg) -> Panel.FixFix msg
verticalListFixFix list =
    let
        width =
            list
                |> List.map Panel.growFixGetMinWidth
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Panel.fixFixCustomize
        { width = width
        , height =
            list
                |> List.map Panel.growFixGetHeight
                |> List.sum
        , nSvgElementList =
            list
                |> List.foldl
                    (\growFixPanel ( offsetY, elementList ) ->
                        ( offsetY + Panel.growFixGetHeight growFixPanel
                        , elementList
                            ++ (Panel.growFixToNSvgElementList growFixPanel { width = width }
                                    |> List.map
                                        (NSvg.translate
                                            { x = 0
                                            , y = offsetY
                                            }
                                        )
                               )
                        )
                    )
                    ( 0, [] )
                |> Tuple.second
        }


{-| 縦並び分割、幅は外に合わせて伸びる
-}
verticalListGrowFix : List (Panel.GrowFix msg) -> Panel.GrowFix msg
verticalListGrowFix list =
    Panel.growFixCustomize
        { height =
            list
                |> List.map Panel.growFixGetHeight
                |> List.sum
        , minWidth =
            list
                |> List.map Panel.growFixGetMinWidth
                |> List.maximum
                |> Maybe.withDefault 0
        , nSvgElementList =
            \{ width } ->
                list
                    |> List.foldl
                        (\growFixPanel ( offsetY, elementList ) ->
                            ( offsetY + Panel.growFixGetHeight growFixPanel
                            , elementList
                                ++ (Panel.growFixToNSvgElementList growFixPanel { width = width }
                                        |> List.map
                                            (NSvg.translate
                                                { x = 0
                                                , y = offsetY
                                                }
                                            )
                                   )
                            )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


type VerticalListItem msg
    = VerticalDivideItemFix (Panel.GrowFix msg)
    | VerticalDivideItemGrow Int (Panel.GrowGrow msg)


{-| 縦並び分割、幅は中身に合わせて、高さは外に合わせて伸びる
-}
verticalListFixGrow : List (VerticalListItem msg) -> Panel.FixGrow msg
verticalListFixGrow list =
    let
        weightSum =
            verticalListWeightSum list

        width =
            verticalListMinWidth list
    in
    Panel.fixGrowCustomize
        { width = width
        , minHeight = verticalListMinHeight list
        , nSvgElementList =
            \{ height } ->
                let
                    rest =
                        verticalListRest height list
                in
                list
                    |> List.foldl
                        (\item ( offsetY, elementList ) ->
                            case item of
                                VerticalDivideItemFix growFixPanel ->
                                    ( offsetY + Panel.growFixGetHeight growFixPanel
                                    , elementList
                                        ++ (Panel.growFixToNSvgElementList growFixPanel { width = width }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = 0
                                                        , y = offsetY
                                                        }
                                                    )
                                           )
                                    )

                                VerticalDivideItemGrow weight growGrowPanel ->
                                    ( offsetY + rest * weight // weightSum
                                    , elementList
                                        ++ (Panel.growGrowToNSvgElementList growGrowPanel
                                                { width = width
                                                , height = rest * weight // weightSum
                                                }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = 0
                                                        , y = offsetY
                                                        }
                                                    )
                                           )
                                    )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


{-| 縦並び分割、幅と高さは外に合わせて伸びる
-}
verticalListGrowGrow : List (VerticalListItem msg) -> Panel.GrowGrow msg
verticalListGrowGrow list =
    Panel.growGrowCustomize
        { minWidth = verticalListMinWidth list
        , minHeight =
            verticalListMinHeight list
        , nSvgElementList =
            \{ width, height } ->
                let
                    rest =
                        verticalListRest height list

                    weightSum =
                        verticalListWeightSum list
                in
                list
                    |> List.foldl
                        (\item ( offsetY, elementList ) ->
                            case item of
                                VerticalDivideItemFix growFixPanel ->
                                    ( offsetY + Panel.growFixGetHeight growFixPanel
                                    , elementList
                                        ++ (Panel.growFixToNSvgElementList
                                                growFixPanel
                                                { width = width }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = 0
                                                        , y = offsetY
                                                        }
                                                    )
                                           )
                                    )

                                VerticalDivideItemGrow weight growGrowPanel ->
                                    ( offsetY + rest * weight // weightSum
                                    , elementList
                                        ++ (Panel.growGrowToNSvgElementList
                                                growGrowPanel
                                                { width = width
                                                , height = rest * weight // weightSum
                                                }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = 0
                                                        , y = offsetY
                                                        }
                                                    )
                                           )
                                    )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


{-| 縦に伸びる縦分割全体の最小の高さ
TODO アルゴリズムを変更? 最小の高さだと中身のパネルがすべて正常に表示できない
-}
verticalListMinHeight : List (VerticalListItem msg) -> Int
verticalListMinHeight =
    List.map
        (\item ->
            case item of
                VerticalDivideItemFix growFixPanel ->
                    Panel.growFixGetHeight growFixPanel

                VerticalDivideItemGrow _ growGrowPanel ->
                    Panel.growGrowGetMinHeight growGrowPanel
        )
        >> List.sum


{-| 縦に伸びるリストの幅。大きいものに合わせる
-}
verticalListMinWidth : List (VerticalListItem msg) -> Int
verticalListMinWidth =
    List.map
        (\item ->
            case item of
                VerticalDivideItemFix growFixPanel ->
                    Panel.growFixGetMinWidth growFixPanel

                VerticalDivideItemGrow _ growGrowPanel ->
                    Panel.growGrowGetMinWidth growGrowPanel
        )
        >> List.maximum
        >> Maybe.withDefault 0


{-| 縦に伸びる縦分割のあまりの高さ
-}
verticalListRest : Int -> List (VerticalListItem msg) -> Int
verticalListRest height list =
    height
        - (list
            |> List.filterMap
                (\item ->
                    case item of
                        VerticalDivideItemFix growFixPanel ->
                            Just (Panel.growFixGetHeight growFixPanel)

                        VerticalDivideItemGrow _ _ ->
                            Nothing
                )
            |> List.sum
          )


{-| 縦に伸びるものの重さの合計
-}
verticalListWeightSum : List (VerticalListItem msg) -> Int
verticalListWeightSum =
    List.filterMap
        (\item ->
            case item of
                VerticalDivideItemFix _ ->
                    Nothing

                VerticalDivideItemGrow weight _ ->
                    Just weight
        )
        >> List.maximum
        >> Maybe.withDefault 0



{-
   =========================================
                   横並びリスト
   =========================================
-}


{-| 横並びリスト。幅高さは中身に合わせる
-}
horizontalListFixFix : List (Panel.FixGrow msg) -> Panel.FixFix msg
horizontalListFixFix list =
    let
        height =
            list
                |> List.map Panel.fixGrowGetMinHeight
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Panel.fixFixCustomize
        { width =
            list
                |> List.map Panel.fixGrowGetWidth
                |> List.sum
        , height =
            height
        , nSvgElementList =
            list
                |> List.foldl
                    (\fixGrowPanel ( offsetX, elementList ) ->
                        ( offsetX + Panel.fixGrowGetWidth fixGrowPanel
                        , elementList
                            ++ (Panel.fixGrowToNSvgElementList
                                    fixGrowPanel
                                    { height = height }
                                    |> List.map
                                        (NSvg.translate
                                            { x = offsetX
                                            , y = 0
                                            }
                                        )
                               )
                        )
                    )
                    ( 0, [] )
                |> Tuple.second
        }


{-| 横並びリスト。高さは外に合わせて伸びる。幅は中身に合わせる
-}
horizontalListFixGrow : List (Panel.FixGrow msg) -> Panel.FixGrow msg
horizontalListFixGrow list =
    Panel.fixGrowCustomize
        { width =
            list
                |> List.map Panel.fixGrowGetWidth
                |> List.sum
        , minHeight =
            list
                |> List.map Panel.fixGrowGetMinHeight
                |> List.maximum
                |> Maybe.withDefault 0
        , nSvgElementList =
            \{ height } ->
                list
                    |> List.foldl
                        (\fixGrowPanel ( offsetX, elementList ) ->
                            ( offsetX + Panel.fixGrowGetWidth fixGrowPanel
                            , Panel.fixGrowToNSvgElementList fixGrowPanel { height = height }
                                |> List.map
                                    (NSvg.translate
                                        { x = offsetX
                                        , y = 0
                                        }
                                    )
                            )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


type HorizontalListItem msg
    = HorizontalListItemFix (Panel.FixGrow msg)
    | HorizontalListItemGrow Int (Panel.GrowGrow msg)


{-| 横並びリスト。幅は外に合わせて伸びる。高さは中身に合わせる
-}
horizontalListGrowFix : List (HorizontalListItem msg) -> Panel.GrowFix msg
horizontalListGrowFix list =
    let
        height =
            horizontalListMinHeight list
    in
    Panel.growFixCustomize
        { minWidth = horizontalListMinWidth list
        , height = height
        , nSvgElementList =
            \{ width } ->
                let
                    rest =
                        horizontalListRest width list

                    weightSum =
                        horizontalListWeightSum list
                in
                list
                    |> List.foldl
                        (\item ( offsetX, elementList ) ->
                            case item of
                                HorizontalListItemFix fixGrow ->
                                    ( offsetX + Panel.fixGrowGetWidth fixGrow
                                    , elementList
                                        ++ (Panel.fixGrowToNSvgElementList fixGrow { height = height }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = offsetX
                                                        , y = 0
                                                        }
                                                    )
                                           )
                                    )

                                HorizontalListItemGrow weight growGrow ->
                                    ( offsetX + weight * rest // weightSum
                                    , elementList
                                        ++ (Panel.growGrowToNSvgElementList growGrow { width = weight * rest // weightSum, height = height }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = offsetX
                                                        , y = 0
                                                        }
                                                    )
                                           )
                                    )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


{-| 横並びリスト。幅と高さは外に合わせて伸びる
-}
horizontalListGrowGrow : List (HorizontalListItem msg) -> Panel.GrowGrow msg
horizontalListGrowGrow list =
    Panel.growGrowCustomize
        { minWidth = horizontalListMinWidth list
        , minHeight = horizontalListMinHeight list
        , nSvgElementList =
            \{ width, height } ->
                let
                    rest =
                        horizontalListRest width list

                    weightSum =
                        horizontalListWeightSum list
                in
                list
                    |> List.foldl
                        (\item ( offsetX, elementList ) ->
                            case item of
                                HorizontalListItemFix fixGrow ->
                                    ( offsetX + Panel.fixGrowGetWidth fixGrow
                                    , elementList
                                        ++ (Panel.fixGrowToNSvgElementList fixGrow { height = height }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = offsetX
                                                        , y = 0
                                                        }
                                                    )
                                           )
                                    )

                                HorizontalListItemGrow weight growGrow ->
                                    ( offsetX + weight * rest // weightSum
                                    , elementList
                                        ++ (Panel.growGrowToNSvgElementList growGrow { width = weight * rest // weightSum, height = height }
                                                |> List.map
                                                    (NSvg.translate
                                                        { x = offsetX
                                                        , y = 0
                                                        }
                                                    )
                                           )
                                    )
                        )
                        ( 0, [] )
                    |> Tuple.second
        }


{-| 横並びリストの最小幅
TODO このアルゴリズムには考察の余地あり
-}
horizontalListMinWidth : List (HorizontalListItem msg) -> Int
horizontalListMinWidth =
    List.map
        (\item ->
            case item of
                HorizontalListItemFix fixGrow ->
                    Panel.fixGrowGetWidth fixGrow

                HorizontalListItemGrow _ growGrow ->
                    Panel.growGrowGetMinWidth growGrow
        )
        >> List.sum


{-| 横並びリストの最小の高さ。大きいものに合わせる
-}
horizontalListMinHeight : List (HorizontalListItem msg) -> Int
horizontalListMinHeight =
    List.map
        (\item ->
            case item of
                HorizontalListItemFix fixGrow ->
                    Panel.fixGrowGetMinHeight fixGrow

                HorizontalListItemGrow _ growGrow ->
                    Panel.growGrowGetMinHeight growGrow
        )
        >> List.maximum
        >> Maybe.withDefault 0


horizontalListRest : Int -> List (HorizontalListItem msg) -> Int
horizontalListRest width list =
    width
        - (list
            |> List.filterMap
                (\item ->
                    case item of
                        HorizontalListItemFix fixGrow ->
                            Just (Panel.fixGrowGetWidth fixGrow)

                        HorizontalListItemGrow _ _ ->
                            Nothing
                )
            |> List.sum
          )


{-| 横に伸びるものの重さの合計
-}
horizontalListWeightSum : List (HorizontalListItem msg) -> Int
horizontalListWeightSum =
    List.filterMap
        (\item ->
            case item of
                HorizontalListItemFix _ ->
                    Nothing

                HorizontalListItemGrow weight _ ->
                    Just weight
        )
        >> List.maximum
        >> Maybe.withDefault 0



{-
   =========================================
                   奥行きリスト
   =========================================
-}


{-|

    奥行きリスト
    パネルを重ねて表現できる。最後の要素が1番手前に表示される

-}
depthList : List (Panel.GrowGrow msg) -> Panel.GrowGrow msg
depthList list =
    Panel.growGrowCustomize
        { minWidth =
            list
                |> List.map Panel.growGrowGetMinWidth
                |> List.maximum
                |> Maybe.withDefault 0
        , minHeight =
            list
                |> List.map Panel.growGrowGetMinHeight
                |> List.maximum
                |> Maybe.withDefault 0
        , nSvgElementList =
            \rec ->
                list
                    |> List.concatMap (\el -> Panel.growGrowToNSvgElementList el rec)
        }
