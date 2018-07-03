{-
  File      : SchellingsModel.hs
  Copyright : (c) Campbell Phalen, 06/6/18
  Contains all functionality for the actual implementation of Schelling's
  Model. All interfacing with simulations, cities, and elements of the model
  will be found here. Dealing with user input and graphics are the dealt with
  elsewhere, otherwise the majority of the implementation is found here.
-}

module SchellingsModel (
	Home(..),
	HomeState(..),
	City(..),
	Simulation(..),
	maybeCity,
	constructSatisfactions,
	constructVacantHomes,
	simulationStep,
	getHome,
	adjustThreshold,
	adjustR,
	satisfactionPercentage,
	randomCity,
	changeSimulationCity,
	setSteps
) where

import Data.Maybe
{- Import Text.Data for readMaybe functionality-}
import Text.Read
import Data.List
{- Module given in an earlier homework assignment that
shuffles a list -}
import Shuffle

{- Datatype that consitutes all of the nodes in our City
datatype. Saves position as well as state to expedite searching -}
data Home = Home {
	state :: HomeState,
	satisfaction :: Float,
	position :: (Int,Int)}
	| NoSuchHome
	deriving(Show)

{- Unique implementation of home that helps us when parsing a city,
NoSuchHome values are equal, and Home values are equal if they have
the same coordinates -}
instance Eq(Home) where
	(==) (Home _ _ position1) (Home _ _ position2) =
		position1 == position2
	(==) NoSuchHome NoSuchHome = True
	(==) _ _ = False

{- Three states that a home can demonstrate -}
data HomeState = Red | Blue | Vacant 
	deriving(Show, Eq)

{- Large part of the Simualation type that saves the grid, I also
include rows and columns fields because I was not aware that the
grid was necessarily a square! -}
data City a = City {rows::Int, columns::Int, grid::[[a]]}
		| NoCity
	deriving(Show,Eq)

{- Mapping acorss a city applies a function to every value in
grid -}
instance Functor(City) where
	fmap f (City rows columns grid) = 
		City rows columns $ map (map f) grid

{- Datatype that specifies every element of a simulation, we also
include NoSimulation for IO if no Simulation can be parsed -}
data Simulation = Simulation {
	neighborhoodR :: Int,
	threshold :: Float,
	steps :: Int,
	vacantHomes :: [Home],
	city :: City Home} | NoSimulation
	deriving(Show,Eq)

{- May or may not return a city from a read in file depending
on if the loaded file had the given specificaitons -}
maybeCity :: [String] -> Maybe (City Home)
maybeCity input@(rowsString:columnsString:grid)
	| not (isValidCity input) = Nothing
	| otherwise = Just $ constructCity input

{- Once we know the grid file is valid, this allows us to
generate the actual city -}
constructCity :: [String] -> City Home
constructCity (rowsString:columnsString:gridString) =
	City rows columns homes where
		states = map readHomeState gridString
		homes = constructHomes columns states
		[rows,columns] = 
			map (\x -> read x :: Int) [rowsString,columnsString]

{- Initializes a single home-}
constructHome :: HomeState -> Int -> Int -> Home
constructHome state row column = 
	Home state 0 (row,column)

{- Reads in the HomeState from string to
HomeState datatype-}
readHomeState :: String -> HomeState
readHomeState state
	| state == "R" = Red
	| state == "B" = Blue
	| state == "O" = Vacant

{- Function used to check if a file is a valid city, essentially
just checks dimensions and makes sure all `words` are readable -}
isValidCity :: [String] -> Bool
isValidCity (rowsString:columnsString:grid)
	| isNothing rows = False
	| isNothing columns = False
	| ((fromJust rows) * (fromJust columns)) /= (length grid) = False
	| otherwise = True where
		[rows,columns] = 
			map (\x -> readMaybe x :: Maybe Int) [rowsString,columnsString]

{- Constructs the two dimensional array of homes from a list of
HomeStates. This manages the dimensions for each home as well -}
constructHomes :: Int -> [HomeState] -> [[Home]]
constructHomes columns states = reverse . snd $ foldl step (0,[]) states where
	step (position, []) state =
		(position + 1, [constructHome state 0 0]:[])
	step (position, accum@(head:rest)) state
		| column == 0 = (position + 1, [home]:accum)
		| otherwise = (position + 1, (head ++ [home]):rest) where
			row = position `div` columns
			column = position `mod` columns
			home = constructHome state row column

{- Helper function which creates the list of vacant homes in a
simulation. Essentially scans the grid looking for vacant homes -}
constructVacantHomes :: Simulation -> Simulation
constructVacantHomes (Simulation r threshold steps _ city@(City _ _ grid)) =
	let vacantHomes = concat $ map (filter isVacant) grid
    in (Simulation r threshold steps vacantHomes city)

{- Very simple function that checks if a home has
state Vacant -}
isVacant :: Home -> Bool
isVacant (Home Vacant _ _) = True
isVacant _ = False

{- Takes a city, the dimensions of the city, and a position,
then returns either NoSuchHome or the Home at the given position -}
getHome :: City Home -> (Int,Int) -> Home
getHome (City rows columns grid) (row,column)
	| not (0 <= row && row < rows) = NoSuchHome
	| not (0 <= column && column < columns) = NoSuchHome
	| otherwise = (grid !! row) !! column

{- Helper function that takes a simulation and recalculates the
satisfactions of every home in the simulation's grid -- useful for
recalcuating simulations after every move -}
constructSatisfactions :: Simulation -> Simulation
constructSatisfactions (Simulation r threshold steps vacants city) =
	Simulation r threshold steps vacants $ fmap (homeSatisfaction city r) city

{- Calculates the satisfaction of any given home in the
specified city-}
homeSatisfaction :: City Home -> Int -> Home -> Home
homeSatisfaction city r home@(Home state _ position) =
	let neighborhood = getNeighborhood city home r
	    s = fromIntegral $ satisfactionIndex state neighborhood
	    h = fromIntegral $ length neighborhood
	in Home state (s/h) position

{- Finds the number of homes surrounding a given home that
are of the same state -}
satisfactionIndex :: HomeState -> [Home] -> Int
satisfactionIndex state homes =
	length $ filter (homeOfState state) homes

{- Checks to see if a given home is of a given state-}
homeOfState :: HomeState -> Home -> Bool
homeOfState compare (Home state _ _) = compare == state
homeOfState _ NoSuchHome = False

{- Calculates the neighborhood of a given home in a given city
with a given radius -}
getNeighborhood :: City Home -> Home -> Int -> [Home]
getNeighborhood city (Home _ _ position) r =
	let allPossibleHomes = nub . map (getHome city) $ getAllPointsAround position r
	    filterFunction = (\x -> not (homeOfState Vacant x) && x /= NoSuchHome)
	in filter filterFunction allPossibleHomes

{- Finds all of the coordinates that are within a given radius
of a center point. Implements some natural recursion -}
getAllPointsAround :: (Int,Int) -> Int -> [(Int,Int)]
getAllPointsAround (row,column) 0 = (row,column):[]
getAllPointsAround (row,column) r =
	(row+r,column+r):
	(row+r,column):
	(row+r,column-r):
	(row,column+r):
	(row,column):
	(row,column-r):
	(row-r,column+r):
	(row-r,column):
	(row-r,column-r):
	getAllPointsAround (row,column) (r-1)

{- Moves a simulation one step further -- if the simulation
is already at an end state no calculations are made -}
simulationStep :: Simulation -> Simulation
simulationStep simulation@(Simulation {city = (City rows columns _)})
	| endStateSimulation simulation = simulation
	| otherwise = runSimulationStep simulation

{- Moves in row-major order across the grid moving unsatisfied 
houses exactly one time. -}
runSimulationStep :: Simulation -> Simulation
runSimulationStep simulation@(Simulation {city = (City rows columns _)}) =
	downStep $ foldl step simulation positionsList where
		step accum position = constructSatisfactions $ moveUnsatisfied accum position
		positionsList = [(r,c) | r <- [0..(rows-1)], c <- [0..(columns-1)]]

{- Checks to see if a simulation is in an end state -- out of steps
or 100% satisfied -}
endStateSimulation :: Simulation -> Bool
endStateSimulation (Simulation {steps = 0}) = True
endStateSimulation simulation =
	1.0 == satisfactionPercentage simulation

{- Returns a float that is the number of satisfied homes
in a city divided by the number of occupied homes total. Used
both to calculate end state and is also displayed -}
satisfactionPercentage :: Simulation -> Float
satisfactionPercentage (Simulation {city = city, threshold = threshold}) =
	let homes = concat . grid $ city
	    notVacants = filter (not . homeOfState Vacant) homes
	    satisfied = filter (homeAboveThreshold threshold) notVacants
	    [numSatisfied,numNotVacant] = map (fromIntegral . length) [satisfied,notVacants]
	in numSatisfied / numNotVacant

{- Decrements the number of steps remaining in a simulation -}
downStep :: Simulation -> Simulation
downStep (Simulation r threshold steps vacantHomes city) =
	(Simulation r threshold (steps-1) vacantHomes city)

{- Checks to see if a home is fit to be moved and
moves if necessary -}
moveUnsatisfied :: Simulation -> (Int,Int) -> Simulation
moveUnsatisfied simulation@(Simulation {threshold = threshold,
										vacantHomes = vacantHomes,
										city = city})
				position
	| (state home) == Vacant = simulation
	| (satisfaction home) >= threshold = simulation
	| moveHome == NoSuchHome = simulation
	| otherwise = relocateHome simulation home moveHome where
		home = getHome city position
		moveHome = findMoveHome simulation home
{- Moves one home to a target home and inserts a vacant home where
the displaced home used to reside -}
relocateHome :: Simulation -> Home -> Home -> Simulation
relocateHome (Simulation r threshold steps vacantHomes city) home move =
	let newVacants = (delete move vacantHomes) ++ [home]
	    movedHomeCity = insertHome city move
	    returnCity = makeVacant movedHomeCity home
	in (Simulation r threshold steps newVacants returnCity)

{- Calculates the ideal place to move an unsatisfied home. This is
the heart of the math specified in the assignment -}
findMoveHome :: Simulation -> Home -> Home
findMoveHome (Simulation r threshold steps vacantHomes city) home =
	let fabricate = fabricateSatisfaction city home r
	    satisfactions = map fabricate vacantHomes
	    thresholds = filter (homeAboveThreshold threshold) satisfactions
	    lowest = lowestSatisfaction thresholds
	in if (lowest == [])
		then NoSuchHome
		else (lowest !! 0)

{- Artifically sets every vacant home to have the state of the unsatisfied
home so that we can calculate the would-be satisfactions and then find
the move ideal -}
fabricateSatisfaction :: City Home -> Home -> Int -> Home -> Home
fabricateSatisfaction city oldHome r newHome =
	let falseHome = setHomeState (state oldHome) newHome
	    falseCity = makeVacant (insertHome city falseHome) oldHome
	in homeSatisfaction falseCity r falseHome

{- Places a home into a city in the given position. The
home that was in that position beforehand is overwritten -}
insertHome :: City Home -> Home -> City Home
insertHome city home =
	fmap (\x -> if x == home then home else x) city

{- Replaces a give position in a city with a vacant home -}
makeVacant :: City Home -> Home -> City Home
makeVacant city target =
	let vacantHome = Home Vacant 0.0 (position target)
	in fmap (\x -> if x == target then vacantHome else x) city

{- Helper function that changes in the state of a given home -}
setHomeState :: HomeState -> Home -> Home
setHomeState newState (Home oldState satisfaction position) =
	Home newState satisfaction position

{- Helper function that checks if a home's satisfaction is
above a given threshold -}
homeAboveThreshold :: Float -> Home -> Bool
homeAboveThreshold threshold (Home _ satisfaction _) =
	satisfaction >= threshold

{- Takes a list of homes and returns the home with lowest
satisfaction -}
lowestSatisfaction :: [Home] -> [Home]
lowestSatisfaction homes =
	let lowest = minimum $ map satisfaction homes
	in filter (\x -> lowest == (satisfaction x)) homes

{- Applies a function to a simulation's threshold. I use this mostly
to apply the change in threshold of 0.05 with user input -}
adjustThreshold :: (Float -> Float) -> Simulation -> Simulation
adjustThreshold adjust (Simulation r threshold steps vacantHomes city)
	| threshold' > 1.0 = (Simulation r 1.0 steps vacantHomes city)
	| threshold' < 0.0 = (Simulation r 0.0 steps vacantHomes city)
	| otherwise = (Simulation r threshold' steps vacantHomes city)
	where
		threshold' = (adjust threshold) 

{- Applies a function to a simulation's neighborhoodR. I use this mostly
to apply the change in R-size of 1 with user input -}
adjustR :: (Int -> Int) -> Simulation -> Simulation
adjustR adjust (Simulation r threshold steps vacantHomes city)
	| r' < 0 = (Simulation 0 threshold steps vacantHomes city)
	| otherwise = (Simulation r' threshold steps vacantHomes city)
	where
		r' = (adjust r)

{- IO function (due to System.Random) that returns a city with
randomized home locations. The city will have percentages as specifed
in the function call. If the number of homes does not add up to the size
of the grid after rounding, the vacant homes are modified so that we have
the correct number of homes -}
randomCity :: Int -> Int -> Int -> Int -> Int -> IO(City Home)
randomCity rows columns red blue vacant = do
	let totalHomes = fromIntegral $ (rows * columns)
	let [numRed,numBlue,numVacant] = map (\x -> (round (((fromIntegral x)/100)*totalHomes)::Int)) [red,blue,vacant]
	let offset = (sum [numRed,numBlue,numVacant]) - (rows * columns)
	let numVacant' = numVacant - offset
	let states = concat [
		replicate numRed Red,
		replicate numBlue Blue,
		replicate numVacant' Vacant]
	randomStates <- shuffle states
	let homes = constructHomes columns randomStates
	return (City rows columns homes)

{- Inserts a new city into a simulation and then recalculates
the satisfactions and vacant homes values-}
changeSimulationCity :: Simulation -> City Home -> Simulation
changeSimulationCity (Simulation r threshold steps vacants _) city' =
	let simulation' = (Simulation r threshold steps vacants city')
	in constructVacantHomes . constructSatisfactions $ simulation'

{- Changes the number of sets in a simulation to the given
integer-}
setSteps :: Int -> Simulation -> Simulation
setSteps steps (Simulation r threshold _ vacants city) =
	(Simulation r threshold steps vacants city)