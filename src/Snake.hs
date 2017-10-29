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
eatOrMove :: Maybe Coord -> Coord -> Snake -> Snake
eatOrMove Nothing         _    snake = snake
eatOrMove (Just nextHead) food snake = snakeHead <| snakeTail
 where
  snakeHead = nextHead
  snakeTail = if nextHead == food
    then snake
    else case S.viewr snake of
      EmptyR -> error "Snakes can't be empty!"
      s:>_   -> s

snakeDiesOnMove :: Maybe Coord -> Snake -> Bool
snakeDiesOnMove Nothing         _     = False
snakeDiesOnMove (Just nextHead) snake = bodyHit || borderHit
 where
  bodyHit   = nextHead `elem` snake
  borderHit = outOfBounds nextHead

getSnakeHead :: Snake -> Coord
getSnakeHead snake = case S.viewl (snake) of
  EmptyL -> error "Snakes can't be empty!"
  s:<_   -> s

-- | Get next head location of the game's snake
calcNextHead :: Direction -> Snake -> Maybe Coord
calcNextHead nextDir snake = go $ S.viewl (snake)
 where
  go (EmptyL) = error "Snakes can't be empty!"
  go (a:<_  ) = case nextDir of
    North -> Just $ a & _y %~ (+1)
    South -> Just $ a & _y %~ (subtract 1)
    East  -> Just $ a & _x %~ (+1)
    West  -> Just $ a & _x %~ (subtract 1)
    NoDir -> Nothing

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

