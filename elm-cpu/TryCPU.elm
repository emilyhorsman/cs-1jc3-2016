-- http://www.cas.mcmaster.ca/~anand/1JC3Pics/CPU.pdf
-- mm's this week go to chinnh@mcmaster.ca (Natalie)


module TryCPU exposing (..)

import RunCPU exposing (..)
import CPU exposing (..)
import Array exposing (Array)
import GraphicSVG exposing (..)
import List exposing (concat, map, map2, foldr, indexedMap, filter, concatMap)
import Time
import Set


--main = show <| (initialState, initialTinyData)


drawRegister : Int -> a -> Shape b
drawRegister index register =
    group
        [ square 18
            |> outlined (solid 1) black
            |> move ( toFloat index * 18, 0 )
        , register
            |> toString
            |> text
            |> centered
            |> filled black
            |> move ( toFloat index * 18, -4 )
        ]


drawRegisters : Int -> List a -> Shape b
drawRegisters index registers =
    List.indexedMap drawRegister registers
        |> group
        |> move ( 18 * -4 + 9, toFloat (index + 1) * 18 )


drawCPUState : Int -> CPUState -> Shape a
drawCPUState index cpuState =
    let
        (CPUState ( r1, r2, r3, r4, r5, r6, r7, r8 ) _ _ _) =
            cpuState

        registers =
            [ r1, r2, r3, r4, r5, r6, r7, r8 ]
    in
        drawRegisters index registers


displayCPUStates : List CPUState -> Shape a
displayCPUStates cpuStateList =
    let
        labels =
            [ "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8" ]

        drawLabel index label =
            label
                |> text
                |> centered
                |> filled black
                |> move ( toFloat index * 18, -4 )

        labelRow =
            List.indexedMap drawLabel labels
                |> group
                |> move ( 18 * -4 + 9, 0 )
    in
        (labelRow :: List.indexedMap drawCPUState cpuStateList)
            |> group


view model =
    collage 500
        500
        [ model.stateOutput |> displayCPUStates |> move ( 0, 22 )
        , model.instrOutput |> move ( 0, -22 )
        , circle 10
            |> filled
                (if isHalted model.cpu then
                    red
                 else
                    green
                )
            |> notifyTap NextInstr
        ]


type Msg
    = NextInstr


update NextInstr model =
    case model.cpu of
        CPUState regs curr cmp Nothing ->
            let
                ( ( newCpu, newDat ), thisInstr ) =
                    case model.program curr of
                        Just i ->
                            ( executeOne i ( model.cpu, model.dat ), toString i )

                        Nothing ->
                            ( ( CPUState regs curr cmp (Just IllegalInstrAddress), model.dat ), "illegal" )
            in
                { model
                    | cpu = newCpu
                    , dat = newDat
                    , stateOutput = newCpu :: model.stateOutput
                    , instrOutput =
                        group
                            [ thisInstr |> text |> centered |> filled green
                            , model.instrOutput |> move ( 0, -18 )
                            ]
                }

        CPUState regs curr cmp (Just halt) ->
            model



-- CPU has stopped


init =
    { cpu = initialState
    , dat = initialData
    , stateOutput = [ initialState ]
    , instrOutput = group []
    , program =
        mkProgram
            [ LoadImmediate 1 7
            , LoadImmediate 2 3
            , LoadImmediate 4 20
            , LoadImmediate 5 4
            , Add 1 2 1
            , Compare 1 4
            , Branch [ LT ] 5
            , Halt
            ]
    }


main =
    notificationsApp { model = init, view = view, update = update }



--main = show <| runProgram (mkProgram [LoadImmediate 1 7
--                                     ,Halt]
--                          )
--                          (initialState, initialTinyData)
--view = collage 600 600 <|
--  [text <| toString initialState]
