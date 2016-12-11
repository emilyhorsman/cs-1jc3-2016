module BitmapCPU exposing (..)

import Array
import Bitmap exposing (drawResult)
import CPU exposing (mkProgram, Instruction(..))
import RunCPU exposing (runProgram, initialState)


program =
    mkProgram
        [ LoadImmediate 1 7
        , Halt
        ]


main =
    runProgram program ( initialState, Array.repeat (64 ^ 2) 0 )
        |> drawResult
