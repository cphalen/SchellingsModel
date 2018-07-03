{-
  File      : GlossInterface.hs
  Copyright : (c) Campbell Phalen, 06/6/18
  Contains all functionality for the GUI element of the Schelling's Model
  program including the gameloop. Implements Graphics.Gloss for graphics interface.
  The Gloss resource can be found at http://hackage.haskell.org/package/gloss-1.12.0.0/docs/Graphics-Gloss.html
-}

module GlossInterface (
    runSimulation
) where

import Graphics.Gloss
{- Import the gameloop functionality as well-}
import Graphics.Gloss.Interface.IO.Game 
import SchellingsModel

{- Datatype that is mostly a simulation, but also
stores some extraneous values that we need for Gloss,
this is the state passed throughout the gameloop. Once
again we have the constants here, which are the values
specified by user input that we will utiliize throughout
the loop-}
data State = State {
    simulation :: Simulation,
    constants :: (City Home,Int,Int,Int,Int),
    elaspedTime :: Float,
    start :: Bool
}   deriving(Show)

{- Constant for screensize -}
screenSize :: (Int, Int)
screenSize = (1200,1024)

{- Constant for fps -}
fps :: Int
fps = 60

{- Constant for wait between steps -}
waitTime :: Float
waitTime = 2.0

{- Constant for side length of a rectangel which
represents a home (they're really squares! So we only
need one side length) -}
rectangleSide :: Float
rectangleSide = 40

{-- Creates the window that our program will run in -}
window :: Display 
window = InWindow "Schelling's Model" screenSize (10, 10)

{- Makes a color for a home depending on its
state and its satisfaction. We represent unsatisfied
states as having a 0.5 alpha value (50% transparent) -}
makeHomeColor :: HomeState -> Bool -> Color
makeHomeColor Red True = makeColor 255.0 0.0 0.0 1.0
makeHomeColor Red False = makeColor 255.0 0.0 0.0 0.5
makeHomeColor Blue True = makeColor 0.0 0.0 255.0 1.0
makeHomeColor Blue False = makeColor 0.0 0.0 255.0 0.5
makeHomeColor Vacant _ = makeColor 255.0 255.0 255.0 0

{- Main gameloop function called in the Smhs file. Here we initialize
a state and begin the loop. -}
runSimulation :: Simulation -> (City Home,Int,Int,Int,Int) -> IO () 
runSimulation initSimulation constants = do
    {- Construct state with simulation constructed from user input, user
    input, a initial time, and an initial paused state -}
    let initState = State initSimulation constants 0.0 False
    playIO window white fps initState GlossInterface.render eventHandler updateLoop
    return ()

{- Enourmous function that creates all of the labels, grids, and rectangles that
will be printed to the screen. Puts all of the different objects in a list and
uses the pictures function to superimpose all of them -}
render :: State -> IO (Picture) 
render (State simulation@(Simulation r threshold currentSteps _ city) constants _ paused) =
    let drawOrientedHomeRectangle = drawHomeRectangle (rows city) (columns city) threshold
        houses = pictures . concat . grid $ fmap drawOrientedHomeRectangle city
        housingGrid = translate 0 60 houses
        lineGrid = translate (-20) (80) $ drawGridLines (rows city) (columns city)
        titleLabel = (translate (-275) (400)) . (scale 0.2 0.2) . text $ "Schelling's Model of Housing Segregation"
        thresholdRounded = (fromIntegral . round . (100 *) $ threshold)
        thresholdLabel = (translate (-450) (-290)) . (scale 0.2 0.2) . text $ "Similar: " ++ show thresholdRounded ++ "%"
        rLabel = (translate (-450) (-260)) . (scale 0.2 0.2) . text $ "R-size: " ++ show r
        gameState = if paused then "paused" else "running"
        stateLabel = (translate (-450) (-320)) . (scale 0.2 0.2) . text $ "State: " ++ gameState
        (_,_,_,_,maxSteps) = constants
        stepsMessage = show  (maxSteps - currentSteps) ++ " of " ++ show maxSteps
        stepsLabel = (translate (-450) (-350)) . (scale 0.2 0.2) . text $ "Steps: " ++ stepsMessage
        satisfaction = (fromIntegral . round . (1000 *) $ satisfactionPercentage simulation) / 10
        satisfactionLabel = (translate (-450) (-380)) . (scale 0.2 0.2) . text $ "Satisfied: " ++ (show satisfaction) ++ "%"
    in (return . pictures) $ [housingGrid,titleLabel,thresholdLabel,rLabel,lineGrid,stateLabel,stepsLabel,satisfactionLabel,instructionsLabels]

{- Handles all of the possible events, Key events and SpecialKey events alike. -}
eventHandler :: Event -> State -> IO (State) 
eventHandler (EventKey (Char key) Up _ _) state@(State simulation constants time paused) =
    case key of
        {- 's' runs one simulation step and resets wait time to 0 -}
        's' -> return $ State (simulationStep simulation) constants 0 paused
        {- 'r' creates an entirely new grid. If the user specified a grid file,
        the grid is reset to that grid, otherwise a new grid with the same percentages
        as the user specified is generated -}
        'r' -> do
            let (City rows columns _) = city simulation
            let (city,red,blue,vacant,maxSteps) = constants
            if (city == NoCity)
                then do
                    {- If user did not use a grid file -}
                    newCity <- randomCity rows columns red blue vacant
                    let simulation' = setSteps maxSteps $ changeSimulationCity simulation newCity
                    return $ State simulation' constants 0 paused
                else do
                    {- If user did use a grid file-}
                    let newCity = city
                    let simulation' = setSteps maxSteps $ changeSimulationCity simulation newCity
                    return $ State simulation' constants 0 paused
        _ -> return (state)
eventHandler (EventKey (SpecialKey key) Up _ _) state@(State simulation constants time paused) =
    let thresholdChange = 0.05 :: Float
        (_,_,_,_,maxSteps) = constants
    in case key of
        {- Modified the threshold depending on user input, can happen anytime -}
        KeyUp -> return $ State (adjustThreshold (\x -> x + thresholdChange) simulation) constants time paused
        KeyDown -> return $ State (adjustThreshold (\x -> x - thresholdChange) simulation) constants time paused
        {- For chaing R-size we first ensure that the simulation has not undergone 
        any simulation steps already -- you can only change R at the beginning -}
        KeyLeft -> if (steps simulation) == maxSteps
            then return $ State (adjustR (\x -> x + 1) simulation) constants time paused
            else return (state)
        KeyRight -> if (steps simulation) == maxSteps
            then return $ State (adjustR (\x -> x - 1) simulation) constants time paused
            else return (state)
        {- Toggles whether the game is paused or not -}
        KeySpace -> return . togglePaused $ state
{- Catch all event handler so random typing will not create errors -}
eventHandler _ state = return (state)  

{- Updates the simulatiion if not paused and if the elapsed time has exceeded
the wait time. If the game is paused then no simulation step is made (keep in mind
that you can simulation step a simulation that is already in end state, the result
is just no change to that simulation) -}
updateLoop :: Float -> State -> IO (State)
updateLoop deltaTime state@(State simulation constants time True) = return (state)
updateLoop deltaTime (State simulation constants time paused)
    | time > waitTime = return $ State (simulationStep simulation) constants 0 paused
    | otherwise = return $ State simulation constants (time+deltaTime) paused

{- Draws a given home rectangle by calculating the center, multiplying the distance
from the center times the constant rectangle scalar, and translating the rectanlge 
to that position-}
drawHomeRectangle :: Int -> Int -> Float -> Home -> Picture
drawHomeRectangle rowsInt columnsInt threshold(Home state satisfaction position@(yInt,xInt)) =
    let [x,y,rows,columns] = map fromInt [xInt,yInt,rowsInt,columnsInt]
        xCenter = columns / 2
        yCenter = rows / 2
        xOffset = (x-xCenter)*rectangleSide
        yOffset = (yCenter-y)*rectangleSide
        translateFromCenter = translate xOffset yOffset
        satisfied = satisfaction >= threshold
        colorHouse = color (makeHomeColor state satisfied)
    in
        translateFromCenter . colorHouse $ squareSolid rectangleSide

{- Draws all of the grid lines by finding the topLeft, bottomLeft,
and bottomRight, then increments across the grid generating
paths that travel between shifts of those three points depending
on the direction -}
drawGridLines :: Int -> Int -> Picture
drawGridLines rowsInt columnsInt =
    let [rows,columns] = map fromInt [rowsInt,columnsInt]
        xCenter = columns / 2
        yCenter = rows / 2
        topLeft = (-(xCenter*rectangleSide),(yCenter*rectangleSide))
        bottomLeft = (-(xCenter*rectangleSide),-(yCenter*rectangleSide))
        bottomRight = ((xCenter*rectangleSide),-(yCenter*rectangleSide))
        stepHorizontal accum@([(x1,y1),(x2,y2)]:xs) n =
            [(x1+rectangleSide,y1),(x2+rectangleSide,y2)]:accum
        verticalPaths = foldl stepHorizontal [[topLeft,bottomLeft]] [1..columnsInt]
        stepVeritcal accum@([(x1,y1),(x2,y2)]:xs) n =
            [(x1,y1+rectangleSide),(x2,y2+rectangleSide)]:accum
        horizontalPaths = foldl stepVeritcal [[bottomLeft,bottomRight]] [1..columnsInt]
    in pictures $ map line $ verticalPaths ++ horizontalPaths

{- Prints all of the instructions! As close to the actual values as possible -}
instructionsLabels :: Picture
instructionsLabels =
    let thresholdInstruction = (translate (-250) (-290)) . (scale 0.12 0.12) . text $ "(Up/Down to adjust threshold)"
        rInstruction = (translate (-250) (-260)) . (scale 0.12 0.12) . text $ "(Left/Right to adjust R-size)"
        stateInstruction = (translate (-250) (-320)) . (scale 0.12 0.12) . text $ "(Space to pause/begin)"
    in pictures [thresholdInstruction,rInstruction,stateInstruction]

{- Helper function because Gloss does not actually include a square picture-}
squareSolid :: Float -> Picture
squareSolid side = rectangleSolid side side

{- Useful in some calcuations of different types, takes
and integer type and makes it a Numeric-}
fromInt :: (Num a) => Int -> a
fromInt x = fromInteger . toInteger $ x

{- Sets the state of the game to paused or unpaused depending
on the previous state -}
togglePaused :: State -> State
togglePaused (State simulation constants time True) =
    (State simulation constants time False)
togglePaused (State simulation constants time False) =
    (State simulation constants time True)