import Data.IORef
import Graphics.UI.GLUT
import System.Random

data MoveType = ROCK | PAPER | SCISSORS | UNKNOWN deriving Show

coordinate :: (GLfloat, GLfloat) -> IO ()
coordinate (x, y) = vertex $ Vertex3 x y 0

renderShape :: PrimitiveMode -> [(GLfloat, GLfloat)] -> IO ()
renderShape shapeType shapeArray = renderPrimitive shapeType $ mapM_ coordinate shapeArray
 
rock :: IO ()
rock = renderShape Polygon [ ((sin (2*pi*k/12)/2), (cos (2*pi*k/12)/2)) | k <- [1..12] ]

paper :: IO ()
paper = renderShape Quads [ (-0.4, 0.6), (0.4, 0.6), (0.4, -0.6), (-0.4, -0.6) ]

scissors :: IO ()
scissors = renderShape Triangles [ ( 0.0,  0.4), ( 0.8,  0.4), (-0.2, 0),
                                   ( 0.0, -0.4), ( 0.8, -0.4), (-0.2, 0),
                                   (-0.5,  0.3), (-0.8,  0.2), (-0.2, 0),
                                   (-0.5, -0.3), (-0.8, -0.2), (-0.2, 0) ]

main :: IO ()
main = do
  initialWindowSize $= (Size 800 800)
  getArgsAndInitialize
  createWindow "Rock Paper Scissors"
  printInstructions

  currentOption <- newIORef 1 --Placeholder to start with

  keyboardMouseCallback $= Just (keyHandler currentOption)
  displayCallback $= display currentOption
  reshapeCallback $= Just reshape  
  idleCallback $= Just (idle currentOption)

  mainLoop

display :: IORef GLfloat -> DisplayCallback
display currOption = do
  clear [ ColorBuffer ]
  currValue <- get currOption
  drawShape currValue
  flush 

drawShape :: GLfloat -> IO ()
drawShape val
  | val == (1 :: GLfloat) = drawRock
  | val == (2 :: GLfloat) = drawPaper
  | val == (3 :: GLfloat) = drawScissors
  | otherwise = doNothing

drawRock :: IO ()
drawRock = do 
  color $ Color3 0.752941 0.752941 (0.752941 :: GLfloat)
  rock

drawPaper :: IO ()
drawPaper = do 
  color $ Color3 1 1 (0 :: GLfloat)
  paper

drawScissors :: IO ()
drawScissors = do 
  color $ Color3 1 0 (0 :: GLfloat)
  scissors

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

idle :: IORef GLfloat -> IdleCallback
idle currOption = do

  currValue <- get currOption
  if(currValue == (4 :: GLfloat))
    then currOption $~! \var -> var - 3
    else currOption $~! (+ 0)

  postRedisplay Nothing

keyHandler :: IORef GLfloat -> KeyboardMouseCallback
keyHandler currOption keyPressed Down _ _ = case keyPressed of
  (Char 'n') -> currOption $~! (+ 1)
  (Char ' ') -> runSimulation currOption
  (Char 'q') -> quitGame
  _ -> return ()
keyHandler _ _ _ _ _ = return ()

runSimulation :: IORef GLfloat -> IO()
runSimulation selectedOption = do
  selectedValue <- get selectedOption
  let myMove = getMoveFromGLfloat selectedValue

  randomNumberForComputerMove <- getRandomNumberWithinRange (1, 3)
  let computerMove = getMoveFromInt randomNumberForComputerMove

  printMoves myMove computerMove

  resolveFight myMove computerMove

getRandomNumberWithinRange :: (Int, Int) -> IO Int
getRandomNumberWithinRange bounds = do
    randomNumber <- randomRIO bounds :: IO Int
    return randomNumber

getMoveFromGLfloat :: GLfloat -> MoveType
getMoveFromGLfloat val
  | val == (1 :: GLfloat) = ROCK
  | val == (2 :: GLfloat) = PAPER
  | val == (3 :: GLfloat) = SCISSORS
  | otherwise = UNKNOWN

getMoveFromInt :: Int -> MoveType
getMoveFromInt val
  | val == 1 = ROCK
  | val == 2 = PAPER
  | val == 3 = SCISSORS
  | otherwise = UNKNOWN

resolveFight :: MoveType -> MoveType -> IO ()
resolveFight UNKNOWN _ = putStrLn "I dunno"

resolveFight ROCK ROCK = putStrLn "You Draw!\n"
resolveFight ROCK PAPER = putStrLn "You Lose!\n"
resolveFight ROCK SCISSORS = putStrLn "You Win!\n"

resolveFight PAPER ROCK = putStrLn "You Win!\n"
resolveFight PAPER PAPER = putStrLn "You Draw!\n"
resolveFight PAPER SCISSORS = putStrLn "You Lose!\n"

resolveFight SCISSORS ROCK = putStrLn "You Lose!\n"
resolveFight SCISSORS PAPER = putStrLn "You Win!\n"
resolveFight SCISSORS SCISSORS = putStrLn "You Draw!\n"
  
printMoves :: MoveType -> MoveType -> IO ()
printMoves myMove computerMove = do
  putStrLn $ "Your move:     " ++ show myMove
  putStrLn $ "Computer move: " ++ show computerMove

quitGame :: IO ()
quitGame = exit

doNothing :: IO ()
doNothing = putStrLn ""

printInstructions :: IO ()
printInstructions = putStrLn "\nInstructions:\n\nPress:\n     n for the next move type\n     space to select the current move\n     Press q to quit.\n" 

{- NOTES 
  $= assigns things to Mutable StateVars in the global space from OpenGL

  All callback state variables contain a Maybe which set to Nothing if there is no callback set.
  If there is a callback, it is wrapped in a Just to indicate a callback exists.

  Flush forces window to display the current set of OpenGL commands we have setup 

  clear [ ColorBuffer ] -> Clears out the graphics color state for a blank canvas

  mainLoop -> GLUT takes control from here, and uses our callback to perform actions
-} 