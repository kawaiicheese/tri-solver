module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List
import Array
import String
import Dict exposing (Dict)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 400 200 (myShapes model)

-----------------------------------------------
---- this is where code from GameSlot goes ----
---- BUT MOVE "import"s above main         ----
-----------------------------------------------

-- (1) define a new type with the states
type Colours = Red | Blue


-- (2) define one or more transition functions
change old = case old of
               Red    -> Blue
               Blue   -> Red

-- (3) add new states to the start of the game
init = { time = 0
       , funcColour1 = Blue
       , funcColour2 = Blue
       , funcColour3 = Blue
       , funcColour4 = Blue
       , funcColour5 = Blue
       , trig1 = "tan(x)"
       , trig2 = "tan(x)"
       , trig3 = "tan(x)"
       , trig4 = "tan(x)"
       , trig5 = "tan(x)"
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
        in
        [ graphPaperCustom 10 1 (rgb 255 102 102) |> makeTransparent 0.5 -- axes and selected coordinate ticks
        , group
            [ trigComp1 |> notifyTap (Click 1) |> move(-100,0)
            , trigComp2 |> notifyTap (Click 2) |> move(-50,0)
            , trigComp3 |> notifyTap (Click 3)
            , trigComp4 |> notifyTap (Click 4) |> move(50,0)
            , trigComp5 |> notifyTap (Click 5) |> move(100,0)
            ]
            |> move (0,50)
        , text "Solve:" |> size 12 |> filled red |> move ( -110, 0)
        , rect 120 50 |> outlined (solid 1) black |> move ( 0, 0 )
        , triangle 8 |> filled red |> move ( 50, 0 )
        , triangle 8 |> filled red |> rotate (degrees -60) |> move ( -50, 0)

        , text "Answer:" |> size 12 |> filled red |> move ( -110, -70)
        , rect 120 50 |> outlined (solid 1) black |> move ( 0, -70 )

        , rect 55 23 |> filled orange |> move ( -33, -127 )
        , text "Check Identity" |> size 8 |> filled white |> move ( -56, -130 )

        , rect 55 23 |> filled orange |> move ( 33, -127 )
        , text "Clear" |> size 8 |> filled white |> move ( 24, -130 )
        ]
                

-- tip:  use a function to simplify colour calculations
myColour c = case c of
               Red    -> rgb 255 0 0
               Blue   -> rgb 0 0 255

-- (6) add new messages for one or more clicks
type Msg = Tick Float GetKeyState
         | Click Int

-- (8) change the game state when you get a click message:
update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     Click n  -> case n of
                                1 ->
                                    { model | funcColour1 = change model.funcColour1 }
                                2 ->
                                    { model | funcColour2 = change model.funcColour2 }
                                3 ->
                                    { model | funcColour3 = change model.funcColour3 }
                                4 ->
                                    { model | funcColour4 = change model.funcColour4 }
                                5 ->
                                    { model | funcColour5 = change model.funcColour5 }
                                otherwise  ->
                                    model
                        
                             


                                 
