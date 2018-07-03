{-
  File      : Smhs.hs
  Copyright : (c) Campbell Phalen, 06/6/18
  Running file with the actual Main function. This handles
  user input, creates a simulation using the SchellingModel module
  and then creates graphics using the GlossInterface module (specifically
  the runSimulation function found in the GlossInterface module)
-}

{- System.Environment for reading command line arguments-}
import System.Environment
import SchellingsModel
import GlossInterface
import Data.Maybe
{- Data.Maybe for isNothing and fromJust functionality -}
import Data.Maybe
{- Text.Read for readMaybe functionality-}
import Text.Read

{- Initializes a first simulation and then
lets the Gloss gameloop do the rest! -}
main :: IO ()
main = do
    (simulation,constants) <- handleArgs
    if simulation == NoSimulation
        then do
            putStrLn "The command line arguments are invalid -- please try again!"
        else do
            {- Important note here that runSimulation is kind of our bridge to
            the Gloss application and the gameloop there, so we have to pass the
            "constants" that we want Gloss to have here -}
            runSimulation simulation constants
    return ()

{- Very messy function full of logic that deals with all of the
users command line arguments. Returns a simulation as per user specifications,
but also a 6-tuple that we call "constants" in the Gloss file. This tuple
allows us to save some of the user input that we will reuse later in the Gloss file-}
handleArgs :: IO (Simulation,(City Home,Int,Int,Int,Int))
handleArgs = do
    args <- getArgs
    if (length args) /= 5
        then do
            return (NoSimulation,(NoCity,0,0,0,0))
        else do
            let (inMaxSteps:inGrid:inRed:inBlue:inVacant:_) = args
            let readMaxSteps = readMaybe inMaxSteps :: Maybe Int
            let readGridSize = readMaybe inGrid :: Maybe Int
            if (isNothing readMaxSteps)
                then do
                    return (NoSimulation,(NoCity,0,0,0,0))
                else do
                    let maxSteps = fromJust readMaxSteps
                    {- First see if the user entered in a grid_size, if not treat the value as path to read from-}
                    if (isJust readGridSize)
                        then do
                            let gridSize = fromJust readGridSize
                            let readRed = readMaybe inRed :: Maybe Int
                            let readBlue = readMaybe inBlue :: Maybe Int
                            let readVacant = readMaybe inVacant :: Maybe Int
                            {- Make sure all values are readable to Float and that the grid size is not too small or too large-}
                            if (length (filter isNothing [readRed,readBlue,readVacant]) == 0) && (5 <= gridSize && gridSize <= 15)
                                then do
                                    let [red,blue,vacant] = map fromJust [readRed,readBlue,readVacant]
                                    city <- randomCity gridSize gridSize red blue vacant
                                    let newSimulation = (Simulation 1 0.5 maxSteps [] city)
                                    let simulation = constructVacantHomes . constructSatisfactions $ newSimulation
                                    return (simulation,(NoCity,red,blue,vacant,maxSteps))
                                else do
                                    return (NoSimulation,(NoCity,0,0,0,0))
                        else do
                            {- Construct the city and the simulation from a file to read in -}
                            grid <- readFile inGrid
                            let readCity = maybeCity . words $ grid
                            if (isJust readCity)
                                then do
                                    let city = fromJust readCity
                                    let newSimulation = (Simulation 1 0.5 maxSteps [] city)
                                    let simulation = constructVacantHomes . constructSatisfactions $ newSimulation
                                    return (simulation,(city,0,0,0,maxSteps))
                                else do
                                    return (NoSimulation,(NoCity,0,0,0,0))