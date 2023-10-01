{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.State
import Data.Array
import Data.List
import System.Random

import Foreign.C.Types

type Field = Array (Int, Int) Bool
data GameState = GameState {
  field :: Field,
  lastTimestamp :: Integer
}

updateField :: Field -> State GameState ()
updateField newField = modify (\s -> s { field = newField })

neighbourOffsets :: [(Int, Int)]
neighbourOffsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

getLivingNeighbours :: Field -> (Int, Int) -> Int
getLivingNeighbours field (i, j) = length neighbours
    where (_, (n, _)) = bounds field
          validIndex (xOffset, yOffset) = i + xOffset >= 0 && (i + xOffset <= n) && j + yOffset >= 0 && j + yOffset <= n
          neighbours = filter (\offset -> validIndex offset && field ! (i + fst offset, j + snd offset)) neighbourOffsets

-- Conways rules
-- 1. any dead cell that has exactly three neighbors will be born
-- 2. any living cell with less than two neighbours dies
-- 3. any living cell with 2 or three neighbours survives
-- 4. any living cell with more than 3 neighbours dies

calcSurvival :: Field -> (Int, Int) -> Bool
calcSurvival field idx
  | not alive && neighbours == 3 = True
  | alive && neighbours < 2 = False
  | alive && neighbours > 3 = False
  | alive = True
  | otherwise = False
      where neighbours = getLivingNeighbours field idx
            alive = field ! idx

conway :: Field -> Field
conway field = array (bounds field) [(idx, calcSurvival field idx) | idx <- indices field]

-- Split a list into sublists of a given length
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n list = take n list : chunk n (drop n list)

getRandomChances :: Int -> IO (Array (Int, Int) Bool)
getRandomChances size = do
  gen <- newStdGen
  let randomNumbers = take (size * size) $ randomRs (1, 100) gen :: [Int]
  let nestedList = chunk size randomNumbers
  return $ listArray ((0, 0), (size - 1, size - 1)) (concatMap (map (< 30)) nestedList)

initialField :: IO Field
initialField = do
  randomChances <- getRandomChances 100
  return $ array ((0, 0), (99, 99)) [((i, j), randomChances ! (i, j)) | i <- [0..99], j <- [0..99]]

windowConfig :: WindowConfig
windowConfig = defaultWindow { windowInitialSize = V2 800 800}

renderCells :: GameState -> Renderer -> IO ()
renderCells state renderer = do
    let currField = field state
        living = filter snd [(idx, currField ! idx) | idx <- indices currField]
        livingRects = map (\((x, y), _) -> let
              xPos = fromIntegral x * 8
              yPos = fromIntegral y * 8
              width = 8
              height = 8
            in Rectangle (P (V2 xPos yPos)) (V2 width height)) living
    rendererDrawColor renderer $= V4 255 255 255 255
    mapM_ (fillRect renderer . Just) livingRects

renderField :: GameState -> Renderer -> IO ()
renderField state renderer = do
    rendererDrawColor renderer $= V4 28 40 51 255
    clear renderer
    renderCells state renderer
    present renderer

updateGameState :: State GameState ()
updateGameState = do
  state <- Control.Monad.State.get
  let newField = conway (field state)
  updateField newField

appLoop:: GameState -> Renderer -> IO ()
appLoop state renderer = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events
    -- update world
    let updatedState = execState updateGameState state
    renderField updatedState renderer
    delay 200
    unless qPressed (appLoop updatedState renderer)

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" windowConfig
    renderer <- createRenderer window (-1) defaultRenderer
    startField <- initialField
    let initialState = GameState { field = startField, lastTimestamp = 0 }
    appLoop initialState renderer
    destroyWindow window
