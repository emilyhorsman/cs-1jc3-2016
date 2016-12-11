module BitmapCPU exposing (..)

import Array
import Bitmap exposing (drawResult, Bitmap)
import CPU exposing (mkProgram, Instruction(..), RegisterNumber, RegisterValue)
import RunCPU exposing (runProgram, initialState)
import Html exposing (Html, pre, text, span)
import Html.Attributes
import String


create : Int -> Bitmap
create length =
    Array.repeat (length ^ 2) 0


{-| Produce machine instructions to draw a circle given an origin and radius.

    Returns a tuple of instructions and ending instruction address.
-}
circle : RegisterValue -> RegisterValue -> RegisterValue -> RegisterValue -> RegisterNumber -> RegisterNumber -> RegisterNumber -> RegisterNumber -> RegisterNumber -> Int -> ( List Instruction, Int )
circle x y radius length xReg yReg regC regD regE startAddress =
    let
        -- 1. Keep x and y pointers, starting at 0.
        -- 2. Compute radius^2.
        -- 3. Compute x pointer and y pointer minus the origin.
        -- 4. Square these and sum.
        -- 5. Go straight to increment (step 11) if outside the circle.
        -- 6. Compute (radius - 1)^2.
        -- 7. Compare the previous square from step 4 to this value from step 6.
        -- 8. Go straight to increment (step 11) if inside this inner circle.
        -- 9. Load an increment register and length register.
        -- 10. Compute the address of the beginning of the row we're plotting,
        --     then store the pixel value (1) at the start of the row plus the
        --     x value.
        -- 11. Increment the x value by 1.
        -- 12. Check if the x value has reached the length of a row, go to step
        --     2 if it hasn't.
        -- 13. Cycle x back to 0 and increment y.
        -- 14. Check if y has reached the length of a column, go to step 2 if
        --     it hasn't.
        instructions =
            [ LoadImmediate xReg 0
            , LoadImmediate yReg 0
              -- Predicate to check if current point is in circle
              -- (x - oX)^2 + (y - oY)^2 `compare` radius^2
            , LoadImmediate regC radius
            , Multiply regC regC regC
            , LoadImmediate regD -x
            , LoadImmediate regE -y
            , Add regD xReg regD
            , Add regE yReg regE
            , Multiply regD regD regD
            , Multiply regE regE regE
            , Add regD regD regE
            , Compare regD regC
              -- If GT (point is outside circle), go straight to increment
            , LoadImmediate regE (startAddress + 25)
            , Branch [ GT, EQ ] regE
              -- Check if the point is inside a smaller radius circle.
              -- We don't want to fill the circle, so this ensures we are on
              -- the outline of the circle.
            , LoadImmediate regC radius
              -- Single pixel outline.
            , LoadImmediate regE -1
            , Add regC regC regE
            , Multiply regC regC regC
            , Compare regD regC
            , LoadImmediate regE (startAddress + 25)
            , Branch [ LT ] regE
              -- Plot.
            , LoadImmediate regC length
            , LoadImmediate regE 1
            , Multiply regD yReg regC
            , Store regE regD xReg
              -- Increment.
            , LoadImmediate regC length
            , LoadImmediate regD 1
            , Add xReg xReg regD
            , Compare xReg regC
            , LoadImmediate regE (startAddress + 2)
              -- x isn't at the end yet so we must still be iterating.
            , Branch [ LT ] regE
              -- If we did hit the end, cycle x back to start of row.
            , LoadImmediate xReg 0
            , Add yReg yReg regD
              -- Check if finished iterating.
            , Compare yReg regC
            , Branch [ LT ] regE
            ]
    in
        ( instructions, startAddress + List.length instructions )


{-| Produce machine instructions to draw a horizontal line given a y-coordinate
    and the left/right x-coordinates.

    Returns a tuple of instructions and ending instruction address.
-}
horizontal : RegisterValue -> RegisterValue -> RegisterValue -> RegisterValue -> RegisterNumber -> RegisterNumber -> RegisterNumber -> RegisterNumber -> Int -> ( List Instruction, Int )
horizontal y x0 x1 length regA regB regC regD startAddress =
    let
        incReg =
            regC

        addrReg =
            regD

        -- 1. Multiply the length of the Bitmap by the y-coordinate to get the
        --    memory address of the start of the row.
        -- 2. Load the left and right x-coordinate into registers and add the
        --    starting memory address of the row to each.
        --
        --    x0 is going to be our pointer to a memory address. It will loop
        --    until it plots through to x1.
        -- 3. We need a register to store the value `1` so that we can increment
        --    a register value and assign a pixel value in memory.
        -- 4. Load the address of this instruction into a register, this is the
        --    start of our loop.
        -- 5. Store the pixel value at the current pointer to memory.
        -- 6. Increment the current pointer.
        -- 7. Compare x0 and x1 to see if the line is done.
        -- 8. If the line is not done, branch back to step 5 using the address
        --    loaded into a register in step (4).
        instructions =
            [ LoadImmediate regC y
            , LoadImmediate regB length
            , Multiply regC regB regC
              --
            , LoadImmediate regA x0
            , LoadImmediate regB x1
            , Add regA regA regC
            , Add regB regB regC
              --
            , LoadImmediate incReg 1
            , LoadImmediate addrReg (startAddress + 9)
              --
            , Store incReg regA 0
            , Add regA regA incReg
            , Compare regA regB
            , Branch [ LT ] addrReg
            ]
    in
        ( instructions, startAddress + List.length instructions )


{-| Produce machine instructions to draw a vertical line given a x-coordinate
    and the lower/upper y-coordinates.

    Returns a tuple of instructions and ending instruction address.
-}
vertical : RegisterValue -> RegisterValue -> RegisterValue -> RegisterValue -> RegisterNumber -> RegisterNumber -> RegisterNumber -> RegisterNumber -> RegisterNumber -> Int -> ( List Instruction, Int )
vertical x y0 y1 length regA regB regC regD regE startAddress =
    let
        pixelReg =
            regD

        addrReg =
            regE

        y0Reg =
            regA

        y1Reg =
            regB

        -- 1. Load the lower and upper y-coordinate into a register.
        -- 2. Multiply each coordinate by the length of the bitmap to get the
        --    memory address of the start of their rows.
        -- 3. Add the x-coordinate to the y0/y1 registers to get the starting
        --    and ending memory address of our loop.
        --
        --    y0 is going to be our pointer to a memory address. It will loop
        --    by incrementing the length of the bitmap until it reaches y1.
        -- 4. Load a register with the pixel value `1`.
        -- 5. Unlike the horizontal line, we cannot re-use the pixel value
        --    register as our increment value. We are incrementing by a row on
        --    the bitmap each time, instead of by a column. Thus we load a
        --    register to store the length of the bitmap.
        -- 6. Load a register with the addreses of this instruction to branch
        --    back to.
        -- 7. Store the pixel value at the current pointer in memory.
        -- 8. Add the increment to y0.
        -- 9. Check if y0 has reached y1 yet.
        -- 10. Loop back to step (7) if the line is not complete yet.
        instructions =
            [ LoadImmediate y0Reg y0
            , LoadImmediate y1Reg y1
            , LoadImmediate regC length
            , Multiply y0Reg y0Reg regC
            , Multiply y1Reg y1Reg regC
              --
            , LoadImmediate regC x
            , Add y0Reg y0Reg regC
            , Add y1Reg y1Reg regC
              --
            , LoadImmediate regC length
            , LoadImmediate pixelReg 1
            , LoadImmediate addrReg (startAddress + 11)
              --
            , Store pixelReg y0Reg 0
            , Add y0Reg y0Reg regC
            , Compare y0Reg y1Reg
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


{-| Many programs use Branch instructions which require knowing the address of
    the instruction we want to go to. When concatenating many of these groups
    together, each group needs to know its starting address, which is based
    on the ending address of the last instruction group.

    This function takes a partially applied function and the current list of
    instructions and start address. It will return a new list of instructions
    with the new ending address.

    Designed to work with a fold call.
-}
concatWithAddr : (Int -> ( List Instruction, Int )) -> ( List Instruction, Int ) -> ( List Instruction, Int )
concatWithAddr command ( list, startAddress ) =
    let
        ( instructions, endAddress ) =
            command startAddress
    in
        ( list ++ instructions, endAddress )


instructions =
    let
        commands =
            [ rect 5 20 5 20 64
            , rect 0 63 0 63 64
            , circle 30 30 15 64 1 2 3 4 5
            ]

        ( instructions, _ ) =
            List.foldl concatWithAddr ( [], 0 ) commands
    in
        [ instructions
        , [ Halt ]
        ]
            |> List.foldl (flip (++)) []


main =
    let
        instructionText =
            instructions
                |> List.map toString
                |> String.join "\n"
                |> text
    in
        span
            []
            [ runProgram (mkProgram instructions) ( initialState, create 64 )
                |> drawResult
            , pre
                [ Html.Attributes.style
                    [ ( "display", "block" )
                    , ( "width", "310px" )
                    , ( "height", "calc(100vh - 350px)" )
                    , ( "font-family", "monospace" )
                    , ( "overflow", "scroll" )
                    , ( "margin", "10px" )
                    ]
                ]
                [ instructionText ]
            ]
