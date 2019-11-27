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

-- Answers = Dict

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
       , block = -1
       , trig1 = "sin(x)"
       , trig2 = "cos(x)"
       , trig3 = "tan(x)"
       , trig4 = "sec(x)"
       , trig5 = "csc(x)"
       , problemVal = Problem1
       , solutionVal = []
       }

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
                    [ rect 320 50 |> outlined (solid 1) black
                    , rect 320 50 |> filled white |> makeTransparent 0.5
                    , text "Solve:" |> size 12 |> filled red |> move ( -155, 10)
                    , text (myProblems <| model.problemVal) |> size 8 |> filled black |> move ( -150, -5)
                    ]
            answerBlock =
                group
                    [ rect 320 50 |> outlined (solid 1) black
                    , rect 320 50 |> filled white |> makeTransparent 0.5
                    , text "Answer:" |> size 12 |> filled red |> move ( -155, 10)
                    , text (mySolution <| model.solutionVal) |> size 8 |> filled black |> move ( -150, -5)
                    ]
            checkButton =
                group
                    [ rect 60 23 |> filled red
                    , text "Check Identity" |> size 8 |> filled white |> move ( -27, -2 )
                    ]
            clearButton =
                group
                    [ rect 55 23 |> filled red
                    , text "Clear" |> size 8 |> filled white |> move ( -10, -2 )
                    ]
        in
        [ group
            [ group
                [ trigComp1 |> notifyTap (ClickC 1) |> move(-100, 15)
                , trigComp2 |> notifyTap (ClickC 2) |> move(-50, 15)
                , trigComp3 |> notifyTap (ClickC 3) |> move(0, 15)
                , trigComp4 |> notifyTap (ClickC 4) |> move(50, 15)
                , trigComp5 |> notifyTap (ClickC 5) |> move(100, 15)
                , addOpp |> notifyTap (ClickC 6) |> move(-75, -15)
                , subOpp |> notifyTap (ClickC 7) |> move(-25, -15)
                , mulOpp |> notifyTap (ClickC 8) |> move(25, -15)
                , divOpp |> notifyTap (ClickC 9) |> move(75, -15)
                , rect 240 15 |> filled black |> makeTransparent 0 |> move(0, model.block * 15)
                ]
                |> move (0,65)
            , group
                [ triangle 20 |> filled red |> notifyTap (ClickPRight) |> move ( 175, 0 )
                , triangle 20 |> filled red |> notifyTap (ClickPLeft) |> rotate (degrees -60) |> move ( -175, 0 )
                ] |> move (0, -35)
            , questionBlock
            , answerBlock |> move(0,-70)
            , checkButton |> notifyTap (ClickCheck) |> move ( -35, -125 )
            , clearButton |> notifyTap (ClickClear) |> move ( 35, -125 )
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

mySolution s = List.foldl (String.append) "" s

-- (6) add new messages for one or more clicks
type Msg = Tick Float GetKeyState
         | ClickC Int
         | ClickPLeft
         | ClickPRight
         | ClickClear
         | ClickCheck

-- (8) change the game state when you get a click message:
update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     ClickC n  -> case n of
                                1 ->
                                    { model |
                                        -- | funcColour1 = changeC model.funcColour1
                                        solutionVal = model.trig1 :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                2 ->
                                    { model |
                                        -- | funcColour2 = changeC model.funcColour2 
                                        solutionVal = model.trig2 :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                3 ->
                                    { model |
                                        -- | funcColour3 = changeC model.funcColour3 
                                        solutionVal = model.trig3 :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                4 ->
                                    { model |
                                        -- | funcColour4 = changeC model.funcColour4 
                                        solutionVal = model.trig4 :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                5 ->
                                    { model |
                                        -- | funcColour5 = changeC model.funcColour5 
                                        solutionVal = model.trig5 :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                6 ->
                                    { model |
                                        -- | operColour1 = changeC model.operColour1 
                                        solutionVal = "+" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                7 ->
                                    { model |
                                        -- | operColour2 = changeC model.operColour2 
                                        solutionVal = "-" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                8 ->
                                    { model |
                                        -- | operColour3 = changeC model.operColour3 
                                        solutionVal = "*" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                9 ->
                                    { model |
                                        -- | operColour4 = changeC model.operColour4 
                                        solutionVal = "/" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                otherwise  ->
                                    model
                     ClickPLeft -> { model 
                                        | problemVal = changePLeft model.problemVal
                                        , block = -1
                                        , funcColour1 = Blue
                                        , funcColour2 = Blue
                                        , funcColour3 = Blue
                                        , funcColour4 = Blue
                                        , funcColour5 = Blue
                                        , operColour1 = Green
                                        , operColour2 = Green
                                        , operColour3 = Green
                                        , operColour4 = Green
                                        , solutionVal = []
                                        }
                     ClickPRight -> {model
                                        | problemVal = changePRight model.problemVal
                                        , block = -1
                                        , funcColour1 = Blue
                                        , funcColour2 = Blue
                                        , funcColour3 = Blue
                                        , funcColour4 = Blue
                                        , funcColour5 = Blue
                                        , operColour1 = Green
                                        , operColour2 = Green
                                        , operColour3 = Green
                                        , operColour4 = Green
                                        , solutionVal = []
                                        }
                     ClickClear -> { model
                                        | block = -1
                                        , funcColour1 = Blue
                                        , funcColour2 = Blue
                                        , funcColour3 = Blue
                                        , funcColour4 = Blue
                                        , funcColour5 = Blue
                                        , operColour1 = Green
                                        , operColour2 = Green
                                        , operColour3 = Green
                                        , operColour4 = Green
                                        , solutionVal = []
                                    }
                     ClickCheck -> model