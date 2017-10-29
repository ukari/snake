{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}

module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (randomRIO)

-- Types

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  | NoDir
  deriving (Eq, Show)

-- Constants

height, width :: Int
height = 20
width = 20

-- Functions

-- | Step forward in time
eatOrMove :: Direction -> Coord -> Snake -> Snake
eatOrMove nextDir food snake = snakeHead <| snakeTail
 where
  snakeHead = nextHead nextDir snake
  snakeTail = if nextHead nextDir snake == food
    then snake
    else case S.viewr snake of
      EmptyR -> error "Snakes can't be empty!"
      s:>_   -> s

snakeDiesOnMove :: Direction -> Snake -> Bool
snakeDiesOnMove nextDir snake = bodyHit || borderHit
 where
  bodyHit   = (nextDir /= NoDir) && (nextHead nextDir snake `elem` snake)
  borderHit = outOfBounds (nextHead nextDir snake)

getSnakeHead :: Snake -> Coord
getSnakeHead snake = case S.viewl (snake) of
  EmptyL -> error "Snakes can't be empty!"
  s:<_   -> s

-- | Get next head location of the game's snake
nextHead :: Direction -> Snake -> Coord
nextHead nextDir snake = go $ S.viewl (snake)
 where
  go (EmptyL) = error "Snakes can't be empty!"
  go (a:<_  ) = case nextDir of
    North -> a & _y %~ (+1)
    South -> a & _y %~ (subtract 1)
    East  -> a & _x %~ (+1)
    West  -> a & _x %~ (subtract 1)
    NoDir -> a

turnDir :: Direction -> Direction -> Maybe Direction
turnDir old new | new == opposite old = Nothing
                | otherwise           = Just new
 where
  opposite North = South
  opposite South = North
  opposite East  = West
  opposite West  = East
  opposite NoDir = NoDir

outOfBounds :: Coord -> Bool
outOfBounds c = any (<1) c || c ^. _x > width || c ^. _y > height

initialSnake :: Snake
initialSnake = S.singleton (V2 10 10)

