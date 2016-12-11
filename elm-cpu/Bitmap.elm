module Bitmap exposing (drawResult, Bitmap)

import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes
import Debug exposing (log)


type alias Bitmap =
    Array Int


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
                    "#f8f8f8"
                )
            ]
            []


drawBitmap : Bitmap -> Svg a
drawBitmap data =
    Array.indexedMap drawPixel data
        |> Array.toList
        |> Svg.svg
            [ Svg.Attributes.width "320"
            , Svg.Attributes.height "320"
            , Svg.Attributes.viewBox "0 0 64 64"
            ]


drawResult : ( a, Bitmap ) -> Svg a
drawResult ( state, data ) =
    let
        foo = log "state" state
    in
        drawBitmap data
