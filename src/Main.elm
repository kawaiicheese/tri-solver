module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List
import Array
import String
import Dict exposing (Dict)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 400 250 (myShapes model)

-----------------------------------------------
---- this is where code from GameSlot goes ----
---- BUT MOVE "import"s above main         ----
-----------------------------------------------

-- (1) define a new type with the states
type Colours = Red | Blue | Green | Orange

type Problems = Problem1 | Problem2 | Problem3


-- (2) define one or more transition functions
changeC old = case old of
            --    Red    -> Blue
               Blue   -> Red
               Green  -> Orange
            --    Orange -> Green
               otherwise -> old

changePLeft old = case old of
                Problem1 -> Problem3
                Problem2 -> Problem1
                Problem3 -> Problem2

changePRight old = case old of
                Problem1 -> Problem2
                Problem2 -> Problem3
                Problem3 -> Problem1

-- (3) add new states to the start of the game
init = { time = 0
       , funcColour1 = Blue
       , funcColour2 = Blue
       , funcColour3 = Blue
       , funcColour4 = Blue
       , funcColour5 = Blue
       , operColour1 = Green
       , operColour2 = Green
       , operColour3 = Green
       , operColour4 = Green
       , trig1 = "tan(x)"
       , trig2 = "tan(x)"
       , trig3 = "tan(x)"
       , trig4 = "tan(x)"
       , trig5 = "tan(x)"
       , problemVal = Problem1
       }

-- cycleProblems tr =
--     case tr of
        


-- (4) use the new state to change how the game looks
--     by using model.colour and model.score
myShapes model = 
        let
            trigComp1 = 
                group
                    [ rect 40 15
                        |> filled (myColour <| model.funcColour1)
                    , text model.trig1
                        |> filled white
                        |> move (-16, -5)
                    ]
            trigComp2 = 
                group
                    [ rect 40 15
                        |> filled (myColour <| model.funcColour2)
                    , text model.trig2
                        |> filled white
                        |> move (-16, -5)
                    ]
            trigComp3 = 
                group
                    [ rect 40 15
                        |> filled (myColour <| model.funcColour3)
                    ,  text model.trig3
                        |> filled white
                        |> move (-16, -5)
                    ]
            trigComp4 = 
                group
                    [ rect 40 15
                        |> filled (myColour <| model.funcColour4)
                    , text model.trig4
                        |> filled white
                        |> move (-16, -5)
                    ]
            trigComp5 = 
                group
                    [ rect 40 15
                        |> filled (myColour <| model.funcColour5)
                    , text model.trig5
                        |> filled white
                        |> move (-16, -5)
                    ]
            addOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour1)
                    , text "+" |> filled black |> move(-3, -4)
                    ]
            subOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour2)
                    , text "-" |> filled black |> move(-2, -3)
                    ]
            mulOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour3)
                    , text "*" |> filled black |> move(-3, -6)
                    ]
            divOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour4)
                    , text "a/b" |> filled black |> move(-9,-4)
                    ]
            questionBlock = 
                group
                    [ text "Solve:" |> size 12 |> filled red |> move ( -110, 0)
                    , rect 120 50 |> outlined (solid 1) black |> move ( 0, 0 )
                    , text (myProblems <| model.problemVal) |> size 12 |> filled red |> move ( -30, -5)
                    , triangle 8 |> filled red |> notifyTap (ClickPRight) |> move ( 50, 0 )
                    , triangle 8 |> filled red |> notifyTap (ClickPLeft) |> rotate (degrees -60) |> move ( -50, 0)
                    ]
            answerBlock =
                group
                    [ text "Answer:" |> size 12 |> filled red |> move ( -110, -70)
                    , rect 120 50 |> outlined (solid 1) black |> move ( 0, -70 )
                    ]
            checkButton =
                group
                    [ rect 60 23 |> filled orange |> move ( -33, -127 )
                    , text "Check Identity" |> size 8 |> filled white |> move ( -60, -130 )
                    ]
            clearButton =
                group
                    [ rect 55 23 |> filled orange |> move ( 33, -127 )
                    , text "Clear" |> size 8 |> filled white |> move ( 24, -130 )
                    ]
        in
        [ graphPaperCustom 10 1 (rgb 255 102 102) |> makeTransparent 0.5 -- axes and selected coordinate ticks
        , group
            [ group
                [ trigComp1 |> notifyTap (ClickC 1) |> move(-100,0)
                , trigComp2 |> notifyTap (ClickC 2) |> move(-50,0)
                , trigComp3 |> notifyTap (ClickC 3)
                , trigComp4 |> notifyTap (ClickC 4) |> move(50,0)
                , trigComp5 |> notifyTap (ClickC 5) |> move(100,0)
                , addOpp |> notifyTap (ClickC 6) |> move(-75, -20)
                , subOpp |> notifyTap (ClickC 7) |> move(-25, -20)
                , mulOpp |> notifyTap (ClickC 8) |> move(25, -20)
                , divOpp |> notifyTap (ClickC 9) |> move(75, -20)
                ]
                |> move (0,60)
            , questionBlock
            , answerBlock
            , checkButton
            , clearButton
            ]
            |> move(0,30)
        ]
                

-- tip:  use a function to simplify colour calculations
myColour c = case c of
               Red    -> rgb 255 0 0
               Blue   -> rgb 0 0 255
               Green  -> rgb 0 255 0
               Orange -> rgb 255 165 0

myProblems p = case p of
                Problem1    -> "Temp 1"
                Problem2    -> "Temp 2"
                Problem3    -> "Temp 3"

-- (6) add new messages for one or more clicks
type Msg = Tick Float GetKeyState
         | ClickC Int
         | ClickPLeft
         | ClickPRight

-- (8) change the game state when you get a click message:
update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     ClickC n  -> case n of
                                1 ->
                                    { model | funcColour1 = changeC model.funcColour1 }
                                2 ->
                                    { model | funcColour2 = changeC model.funcColour2 }
                                3 ->
                                    { model | funcColour3 = changeC model.funcColour3 }
                                4 ->
                                    { model | funcColour4 = changeC model.funcColour4 }
                                5 ->
                                    { model | funcColour5 = changeC model.funcColour5 }
                                6 ->
                                    { model | operColour1 = changeC model.operColour1 }
                                7 ->
                                    { model | operColour2 = changeC model.operColour2 }
                                8 ->
                                    { model | operColour3 = changeC model.operColour3 }
                                9 ->
                                    { model | operColour4 = changeC model.operColour4 }
                                otherwise  ->
                                    model
                     ClickPLeft -> {model | problemVal = changePLeft model.problemVal}
                     ClickPRight -> {model | problemVal = changePRight model.problemVal}
                             


                                 
