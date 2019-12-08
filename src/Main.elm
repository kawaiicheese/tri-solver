{-

Group 12

Members:
Matthew Po  400007877
Joseph Lu   400022356
Stanley Liu 001404020

-}
module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List
import Array
import String
import Dict exposing (Dict)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 400 250 (myShapes model)

-- define new types
---- Colours that are used
---- Three sample Problems
type Colours = Red | Blue | Green | White
type Problems = Problem1 | Problem2 | Problem3


-- Change
changeC old = case old of
               Blue   -> Red
               otherwise -> old

-- Go to the Previous Problem
changePLeft old = case old of
                Problem1 -> Problem3
                Problem2 -> Problem1
                Problem3 -> Problem2

-- Go to the Next Problem
changePRight old = case old of
                Problem1 -> Problem2
                Problem2 -> Problem3
                Problem3 -> Problem1


-- Initial States
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
       , block = -1 -- Prevents two Trig or two operators in a row
       , trig1Block = 1
       , trig2Block = 1
       , trig3Block = 1
       , trig4Block = 1
       , trig5Block = 1
       , problem = Problem1
       , solutionVal = []
       , solutionC = White
       }

-- Shapes
myShapes model = 
        let
            trigComp1 = 
                group
                    [ rect 55 15
                        |> filled (myColour <| model.funcColour1)
                    , text (myTrig 1 <| myProblems model.problem)
                        |> filled white
                        |> move (-25, -5)
                    ]
            trigComp2 = 
                group
                    [ rect 55 15
                        |> filled (myColour <| model.funcColour2)
                    , text (myTrig 2 <| myProblems model.problem)
                        |> filled white
                        |> move (-25, -5)
                    ]
            trigComp3 = 
                group
                    [ rect 55 15
                        |> filled (myColour <| model.funcColour3)
                    ,  text (myTrig 3 <| myProblems model.problem)
                        |> filled white
                        |> move (-25, -5)
                    ]
            trigComp4 = 
                group
                    [ rect 55 15
                        |> filled (myColour <| model.funcColour4)
                    , text (myTrig 4 <| myProblems model.problem)
                        |> filled white
                        |> move (-25, -5)
                    ]
            trigComp5 = 
                group
                    [ rect 55 15
                        |> filled (myColour <| model.funcColour5)
                    , text (myTrig 5 <| myProblems model.problem)
                        |> filled white
                        |> move (-25, -5)
                    ]
            addOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour1)
                    , text "+" |> filled black |> move( -3, -4 )
                    ]
            subOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour2)
                    , text "-" |> filled black |> move( -2, -3 )
                    ]
            mulOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour3)
                    , text "*" |> filled black |> move( -3, -6 )
                    ]
            divOpp =
                group
                    [ roundedRect 30 15 10 |> filled (myColour <| model.operColour4)
                    , text "/" |> filled black |> move( -2,-4 )
                    ]
            questionBlock = 
                group
                    [ rect 320 50 |> outlined (solid 1) black
                    , rect 320 50 |> filled white |> makeTransparent 0.5
                    , text "Solve:" |> size 12 |> filled red |> move ( -155, 10 )
                    , text ( myTrig 0 <| myProblems model.problem ) |> size 8 |> filled black |> move ( -150, -5 )
                    ]
            answerBlock =
                group
                    [ rect 320 50 |> outlined (solid 1) black
                    , rect 320 50 |> filled (myColour <| model.solutionC) |> makeTransparent 0.5
                    , text "Answer:" |> size 12 |> filled red |> move ( -155, 10 )
                    , text (mySolution <| model.solutionVal) |> size 8 |> filled black |> move ( -150, -5 )
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
                [ trigComp1 |> notifyTap (ClickC 1) |> move(-120, 15)
                , trigComp2 |> notifyTap (ClickC 2) |> move(-60, 15)
                , trigComp3 |> notifyTap (ClickC 3) |> move(0, 15)
                , trigComp4 |> notifyTap (ClickC 4) |> move(60, 15)
                , trigComp5 |> notifyTap (ClickC 5) |> move(120, 15)
                , addOpp |> notifyTap (ClickC 6) |> move(-75, -15)
                , subOpp |> notifyTap (ClickC 7) |> move(-25, -15)
                , mulOpp |> notifyTap (ClickC 8) |> move(25, -15)
                , divOpp |> notifyTap (ClickC 9) |> move(75, -15)
                -- Prevents adding trig or operator twice in a row
                , rect 295 15 |> filled black |> makeTransparent 0 |> move(0, model.block * 15)
                -- Allows each triginometric block to be selected once
                , rect 55 15 |> filled black |> makeTransparent 0 |> move(-120, model.trig1Block * 30)
                , rect 55 15 |> filled black |> makeTransparent 0 |> move(-60, model.trig2Block * 30)
                , rect 55 15 |> filled black |> makeTransparent 0 |> move(0, model.trig3Block * 30)
                , rect 55 15 |> filled black |> makeTransparent 0 |> move(60, model.trig4Block * 30)
                , rect 55 15 |> filled black |> makeTransparent 0 |> move(120, model.trig5Block * 30)
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
                
-- simplify colours
myColour c = case c of
               Red    -> rgb 255 0 0
               Blue   -> rgb 0 0 255
               Green  -> rgb 0 255 0
               White -> GraphicSVG.white

myTrig n p = Maybe.withDefault "1" <| List.head <| List.drop n p

-- simplify problems
myProblems p = case p of
                Problem1    -> ["cos(x)","sin(x)","1","tan(x)","sec(x)","csc(x)"]
                Problem2    -> ["sec(x)^2","1","sin^2(x)","tan^2(x)","sec(x)","cos^2(-x)"]
                Problem3    -> ["cos(2x)","sin^2(x)","1","2","cos^2(x)","tan(x)"]

-- simplify results (hard coded)
solve p s = case (p,s) of
                (Problem1,["tan(x)","/","sin(x)"]) -> Green
                (Problem1,["sec(x)","/","1"]) -> Green

                (Problem2,["1","+","tan^2(x)"]) -> Green
                (Problem2,["tan^2(x)","+","1"]) -> Green
                (Problem2,["cos^2(-x)","/","1"]) -> Green
                (Problem2,["sin^2(x)","/","tan^2(x)"]) -> Green

                (Problem3,["sin^2(x)","-","cos^2(x)"]) -> Green
                (Problem3,["1","-","cos^2(x)","*","2"]) -> Green
                (Problem3,["sin^2(x)","*","2","-","1"]) -> Green

                otherwise   -> Red

-- show Results
mySolution s = List.foldl (String.append) "" s

-- functions called in update
type Msg = Tick Float GetKeyState
         | ClickC Int
         | ClickPLeft
         | ClickPRight
         | ClickClear
         | ClickCheck

update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     ClickC n  -> case n of
                                1 ->
                                    { model
                                        | funcColour1 = changeC model.funcColour1
                                        , solutionVal = (myTrig 1 <| myProblems model.problem) :: model.solutionVal
                                        , trig1Block = 0.5
                                        , block = -1 * model.block
                                    }
                                2 ->
                                    { model
                                        | funcColour2 = changeC model.funcColour2 
                                        , solutionVal = (myTrig 2 <| myProblems model.problem) :: model.solutionVal
                                        , trig2Block = 0.5
                                        , block = -1 * model.block
                                    }
                                3 ->
                                    { model
                                        | funcColour3 = changeC model.funcColour3 
                                        , solutionVal = (myTrig 3 <| myProblems model.problem):: model.solutionVal
                                        , trig3Block = 0.5
                                        , block = -1 * model.block
                                    }
                                4 ->
                                    { model
                                        | funcColour4 = changeC model.funcColour4 
                                        , solutionVal = (myTrig 4 <| myProblems model.problem) :: model.solutionVal
                                        , trig4Block = 0.5
                                        , block = -1 * model.block
                                    }
                                5 ->
                                    { model
                                        | funcColour5 = changeC model.funcColour5 
                                        , solutionVal = (myTrig 5 <| myProblems model.problem) :: model.solutionVal
                                        , trig5Block = 0.5
                                        , block = -1 * model.block
                                    }
                                6 ->
                                    { model |
                                        solutionVal = "+" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                7 ->
                                    { model |
                                        solutionVal = "-" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                8 ->
                                    { model |
                                        solutionVal = "*" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                9 ->
                                    { model | solutionVal = "/" :: model.solutionVal
                                        , block = -1 * model.block
                                    }
                                otherwise  ->
                                    model
                     ClickPLeft -> { model 
                                        | problem = changePLeft model.problem
                                        , block = -1
                                        , trig1Block = 1
                                        , trig2Block = 1
                                        , trig3Block = 1
                                        , trig4Block = 1
                                        , trig5Block = 1
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
                                        , solutionC = White
                                        }
                     ClickPRight -> {model
                                        | problem = changePRight model.problem
                                        , block = -1
                                        , trig1Block = 1
                                        , trig2Block = 1
                                        , trig3Block = 1
                                        , trig4Block = 1
                                        , trig5Block = 1
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
                                        , solutionC = White
                                        }
                     ClickClear -> { model
                                        | block = -1
                                        , trig1Block = 1
                                        , trig2Block = 1
                                        , trig3Block = 1
                                        , trig4Block = 1
                                        , trig5Block = 1
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
                                        , solutionC = White
                                    }
                     ClickCheck -> { model | solutionC = solve model.problem model.solutionVal}