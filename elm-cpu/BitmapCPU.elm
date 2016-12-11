module BitmapCPU exposing (..)

import Array
import Bitmap exposing (drawResult, Bitmap)
import CPU exposing (mkProgram, Instruction(..))
import RunCPU exposing (runProgram, initialState)
import Debug exposing (log)


create : Int -> Bitmap
create length =
    Array.repeat (length ^ 2) 0


horizontal y x0 x1 length regA regB regC regD startAddress =
    let
        incReg =
            regC

        addrReg =
            regD

        instructions =
            [ LoadImmediate regC y
            , LoadImmediate regB length
            , Multiply regC regB regC  -- length * y
            , LoadImmediate regA x0
            , LoadImmediate regB x1
            , Add regA regA regC       -- Starting memory address
            , Add regB regB regC       -- Ending memory address
            , LoadImmediate incReg 1   -- Pixel value and increment
            , LoadImmediate addrReg (startAddress + 9)
            , Store incReg regA 0
            , Add regA regA incReg
            , Compare regA regB
            , Branch [ LT ] addrReg
            ]
    in
        ( instructions, startAddress + List.length instructions )


vertical x y0 y1 length regA regB regC regD regE startAddress =
    let
        pixelReg =
            regD

        addrReg =
            regE

        instructions =
            [ LoadImmediate regA y0
            , LoadImmediate regB y1
            , LoadImmediate regC length
            , Multiply regA regA regC
            , Multiply regB regB regC
            , LoadImmediate regC x
            , Add regA regA regC
            , Add regB regB regC
            , LoadImmediate regC length
            , LoadImmediate pixelReg 1  -- Pixel value
            , LoadImmediate addrReg (startAddress + 11)
            , Store pixelReg regA 0
            , Add regA regA regC
            , Compare regA regB
            , Branch [ LT, EQ ] addrReg
            ]
    in
        ( instructions, startAddress + List.length instructions )


rect x0 x1 y1 y2 length startAddress =
    let
        commands =
            [ horizontal y1 x0 x1 length 1 2 3 4
            , horizontal y2 x0 x1 length 1 2 3 4
            , vertical x0 y1 y2 length 1 2 3 4 5
            , vertical x1 y1 y2 length 1 2 3 4 5
            ]
    in
        List.foldl concatWithAddr ( [], startAddress ) commands


concatWithAddr command ( list, startAddress ) =
    let
        ( instructions, endAddress ) =
            command startAddress
    in
        ( list ++ instructions, endAddress )


program =
    let
        commands =
            [ rect 5 20 5 20 64
            , rect 0 63 0 63 64
            , rect 5 58 40 45 64
            ]

        ( instructions, _ ) =
            List.foldl concatWithAddr ( [], 0 ) commands
    in
        [ instructions
        , [ Halt ]
        ]
            |> List.foldl (flip (++)) []
            |> log "Instructions"
            |> mkProgram

main =
    runProgram program ( initialState, create 64 )
        |> drawResult
