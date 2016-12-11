module BitmapCPU exposing (..)

import Array
import Bitmap exposing (drawResult, Bitmap)
import CPU exposing (mkProgram, Instruction(..))
import RunCPU exposing (runProgram, initialState)


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
            [ LoadImmediate regA y
            , LoadImmediate regB length
            , Multiply regB regA regB  -- length * y
            , LoadImmediate incReg 1   -- Pixel value and increment
            , LoadImmediate regA x0
            , Add regA regA regB       -- Starting memory address
            , LoadImmediate regB x1
            , Add regB regA regB       -- Ending memory address
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
            ]

        ( instructions, _ ) =
            List.foldl concatWithAddr ( [], 0 ) commands
    in
        [ instructions
        , [ Halt ]
        ]
            |> List.foldl (flip (++)) []
            |> mkProgram

main =
    runProgram program ( initialState, create 64 )
        |> drawResult
