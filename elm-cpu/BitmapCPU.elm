module BitmapCPU exposing (..)

import Array
import Bitmap exposing (drawResult, Bitmap)
import CPU exposing (mkProgram, Instruction(..))
import RunCPU exposing (runProgram, initialState)
import Debug exposing (log)


create : Int -> Bitmap
create length =
    Array.repeat (length ^ 2) 0


horizontal startAddress yReg x0Reg x1Reg lengthReg =
    [ Multiply yReg yReg lengthReg
    , Add x0Reg x0Reg yReg
    , Add x1Reg x1Reg yReg
    , LoadImmediate lengthReg 1  -- Using lengthReg as pixel value
    , LoadImmediate yReg (startAddress + 5)
    , Store lengthReg x0Reg 0
    , Add x0Reg x0Reg lengthReg  -- We can re-use the pixel value as increment
    , Compare x0Reg x1Reg
    , Branch [ LT, EQ ] yReg
    ]


program =
    [ [ LoadImmediate 1 1
      , LoadImmediate 2 0
      , LoadImmediate 3 63
      , LoadImmediate 4 64
      ]
    , horizontal 4 1 2 3 4
    , [ LoadImmediate 1 2
      , LoadImmediate 2 1
      , LoadImmediate 3 62
      , LoadImmediate 4 64
      ]
    , horizontal 17 1 2 3 4
    , [ Halt ]
    ]
        |> List.foldl (flip (++)) []
        |> mkProgram

main =
    runProgram program ( initialState, create 64 )
        |> drawResult
