module PanelSample exposing (HorizontalListItem(..), VerticalListItem(..), colorFixFix, colorFixGrow, colorGrowFix, colorGrowGrow, depthList, horizontalListFitFit, horizontalListFitGrow, horizontalListGrowFit, horizontalListGrowGrow, horizontalListMinWidth, horizontalListRest, horizontalListWeightSum, imageContain, imageCover, verticalListFitFit, verticalListFitGrow, verticalListGrowFit, verticalListGrowGrow, verticalListMinHeight, verticalListRest, verticalListWeightSum)

import Color exposing (Color)
import NSvg exposing (Svg)
import Palette.X11 as X
import Panel



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
colorGrowFix : { height : Int } -> Color -> Panel.GrowFit msg
colorGrowFix { height } color =
    Panel.growFitCustomize
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
colorFixGrow : { width : Int } -> Color -> Panel.FitGrow msg
colorFixGrow { width } color =
    Panel.fitGrowCustomize
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
colorFixFix : { width : Int, height : Int } -> Color -> Panel.FitFit msg
colorFixFix { width, height } color =
    Panel.fitFitCustomize
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
    keepAspectImagePanelFromWidth : {width : Int} -> Image -> PanelFitFit msg
    keepAspectImagePanelFromHeight : {height: Int} -> Image -> PanelFitFit msg
    を作りたかった
    Elmでは無理なのでDefinyで挑戦したい
   =========================================
-}


{-|

    画像パネル
    画像を{width,height}で指定した比率に合うようにレターボックス(透明の帯)を上下、左右の足りない方に追加する

-}
imageContain : { width : Int, height : Int } -> String -> Panel.FitFit msg
imageContain { width, height } source =
    Panel.fitFitCustomize
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
imageCover : { width : Int, height : Int } -> String -> Panel.FitFit msg
imageCover { width, height } source =
    Panel.fitFitCustomize
        { width = width
        , height = height
        , nSvgElementList =
            [ NSvg.imageCover
                { width = width, height = height, sourceUrl = source }
            ]
        }



{-
   =========================================
                   縦並びリスト
   =========================================
-}


{-| 縦並び分割、幅と高さは中身に合わせる
-}
verticalListFitFit : List (Panel.GrowFit msg) -> Panel.FitFit msg
verticalListFitFit list =
    let
        width =
            list
                |> List.map Panel.growFitGetMinWidth
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Panel.fitFitCustomize
        { width = width
        , height =
            list
                |> List.map Panel.growFitGetHeight
                |> List.sum
        , nSvgElementList =
            list
                |> List.foldl
                    (\growFitPanel ( offsetY, elementList ) ->
                        ( offsetY + Panel.growFitGetHeight growFitPanel
                        , elementList
                            ++ (Panel.growFitToNSvgElementList growFitPanel { width = width }
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
verticalListGrowFit : List (Panel.GrowFit msg) -> Panel.GrowFit msg
verticalListGrowFit list =
    Panel.growFitCustomize
        { height =
            list
                |> List.map Panel.growFitGetHeight
                |> List.sum
        , minWidth =
            list
                |> List.map Panel.growFitGetMinWidth
                |> List.maximum
                |> Maybe.withDefault 0
        , nSvgElementList =
            \{ width } ->
                list
                    |> List.foldl
                        (\growFitPanel ( offsetY, elementList ) ->
                            ( offsetY + Panel.growFitGetHeight growFitPanel
                            , elementList
                                ++ (Panel.growFitToNSvgElementList growFitPanel { width = width }
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
    = VerticalDivideItemFit (Panel.GrowFit msg)
    | VerticalDivideItemGrow Int (Panel.GrowGrow msg)


{-| 縦並び分割、幅は中身に合わせて、高さは外に合わせて伸びる
-}
verticalListFitGrow : List (VerticalListItem msg) -> Panel.FitGrow msg
verticalListFitGrow list =
    let
        weightSum =
            verticalListWeightSum list

        width =
            verticalListMinWidth list
    in
    Panel.fitGrowCustomize
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
                                VerticalDivideItemFit growFitPanel ->
                                    ( offsetY + Panel.growFitGetHeight growFitPanel
                                    , elementList
                                        ++ (Panel.growFitToNSvgElementList growFitPanel { width = width }
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
                                VerticalDivideItemFit growFitPanel ->
                                    ( offsetY + Panel.growFitGetHeight growFitPanel
                                    , elementList
                                        ++ (Panel.growFitToNSvgElementList
                                                growFitPanel
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
                VerticalDivideItemFit growFitPanel ->
                    Panel.growFitGetHeight growFitPanel

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
                VerticalDivideItemFit growFitPanel ->
                    Panel.growFitGetMinWidth growFitPanel

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
                        VerticalDivideItemFit growFitPanel ->
                            Just (Panel.growFitGetHeight growFitPanel)

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
                VerticalDivideItemFit _ ->
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
horizontalListFitFit : List (Panel.FitGrow msg) -> Panel.FitFit msg
horizontalListFitFit list =
    let
        height =
            list
                |> List.map Panel.fitGrowGetMinHeight
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Panel.fitFitCustomize
        { width =
            list
                |> List.map Panel.fitGrowGetWidth
                |> List.sum
        , height =
            height
        , nSvgElementList =
            list
                |> List.foldl
                    (\fitGrowPanel ( offsetX, elementList ) ->
                        ( offsetX + Panel.fitGrowGetWidth fitGrowPanel
                        , elementList
                            ++ (Panel.fitGrowToNSvgElementList
                                    fitGrowPanel
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
horizontalListFitGrow : List (Panel.FitGrow msg) -> Panel.FitGrow msg
horizontalListFitGrow list =
    Panel.fitGrowCustomize
        { width =
            list
                |> List.map Panel.fitGrowGetWidth
                |> List.sum
        , minHeight =
            list
                |> List.map Panel.fitGrowGetMinHeight
                |> List.maximum
                |> Maybe.withDefault 0
        , nSvgElementList =
            \{ height } ->
                list
                    |> List.foldl
                        (\fitGrowPanel ( offsetX, elementList ) ->
                            ( offsetX + Panel.fitGrowGetWidth fitGrowPanel
                            , Panel.fitGrowToNSvgElementList fitGrowPanel { height = height }
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
    = HorizontalListItemFit (Panel.FitGrow msg)
    | HorizontalListItemGrow Int (Panel.GrowGrow msg)


{-| 横並びリスト。幅は外に合わせて伸びる。高さは中身に合わせる
-}
horizontalListGrowFit : List (HorizontalListItem msg) -> Panel.GrowFit msg
horizontalListGrowFit list =
    let
        height =
            horizontalListMinHeight list
    in
    Panel.growFitCustomize
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
                                HorizontalListItemFit fitGrow ->
                                    ( offsetX + Panel.fitGrowGetWidth fitGrow
                                    , elementList
                                        ++ (Panel.fitGrowToNSvgElementList fitGrow { height = height }
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
                                HorizontalListItemFit fitGrow ->
                                    ( offsetX + Panel.fitGrowGetWidth fitGrow
                                    , elementList
                                        ++ (Panel.fitGrowToNSvgElementList fitGrow { height = height }
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
                HorizontalListItemFit fitGrow ->
                    Panel.fitGrowGetWidth fitGrow

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
                HorizontalListItemFit fitGrow ->
                    Panel.fitGrowGetMinHeight fitGrow

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
                        HorizontalListItemFit fitGrow ->
                            Just (Panel.fitGrowGetWidth fitGrow)

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
                HorizontalListItemFit _ ->
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
