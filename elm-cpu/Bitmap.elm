module Bitmap exposing (drawResult)

import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes


drawPixel : Int -> Int -> Svg a
drawPixel index pixel =
    let
        x =
            index % 64

        y =
            index // 64
    in
        Svg.rect
            [ Svg.Attributes.x (toString x)
            , Svg.Attributes.y (toString y)
            , Svg.Attributes.width "1"
            , Svg.Attributes.height "1"
            , Svg.Attributes.fill
                (if pixel == 1 then
                    "black"
                 else
                    "purple"
                )
            ]
            []


drawBitmap : Array Int -> Svg a
drawBitmap data =
    Array.indexedMap drawPixel data
        |> Array.toList
        |> Svg.svg
            [ Svg.Attributes.width "320"
            , Svg.Attributes.height "320"
            , Svg.Attributes.viewBox "0 0 64 64"
            ]


drawResult : ( a, Array Int ) -> Svg a
drawResult ( _, data ) =
    drawBitmap data
