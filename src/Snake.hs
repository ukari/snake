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

data Game = Game
  { _snake  :: Snake     -- ^ snake as a sequence of points in R2
  , _dir    :: Direction -- ^ direction
  , _food   :: Coord     -- ^ location of the food
  , _dead   :: Bool      -- ^ game over flag
  , _paused :: Bool      -- ^ paused flag
  , _score  :: Int       -- ^ score
  , _frozen :: Bool      -- ^ freeze to disallow duplicate turns between time steps
  } deriving (Eq, Show)

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 20

-- Functions

-- | Step forward in time
step :: Game -> IO Game
step g = fromMaybe (return g) $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  die g' <|> eatFood g' <|> move g'

-- | Possibly die if next head position is disallowed
die :: Game -> Maybe (IO Game)
die g = [ return $ g & dead .~ True | bodyHit || borderHit ]
 where
  bodyHit   = nh g `elem` g ^. snake
  borderHit = outOfBounds (nh g)

-- | Possibly eat food if next head position is food
eatFood :: Game -> Maybe (IO Game)
eatFood g =
  [ do
      let ng = g & score %~ (+10) & snake %~ (nh g<|)
      nf <- nextFood ng
      return $ ng & food .~ nf
  | nh g == g ^. food
  ]

-- | Move snake along in a marquee fashion
move :: Game -> Maybe (IO Game)
move g = Just $ return $ g & snake %~ (mv . S.viewr)
 where
  mv (EmptyR) = error "Snakes can't be empty!"
  mv (s:>_  ) = nh g <| s

-- | Get next head location of the game's snake
nh :: Game -> Coord
nh g = nextHead (g ^. dir) (g ^. snake)

-- | Get next head position of a snake in a particular direction
nextHead :: Direction -> Snake -> Coord
nextHead d = go . S.viewl
 where
  go (EmptyL) = error "Snakes can't be empty!"
  go (a:<_  ) = case d of
    North -> a & _y %~ (+1)
    South -> a & _y %~ (subtract 1)
    East  -> a & _x %~ (+1)
    West  -> a & _x %~ (subtract 1)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game
turn d g = if g ^. frozen
  then g
  else g & dir %~ (turnDir d) & paused .~ False & frozen .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | n == opposite c = c
            | otherwise       = n
 where
  opposite North = South
  opposite South = North
  opposite East  = West
  opposite West  = East

outOfBounds :: Coord -> Bool
outOfBounds c = any (<1) c || c ^. _x > width || c ^. _y > height

-- | Get a valid next food coordinate
nextFood :: Game -> IO Coord
nextFood g = do
  c <- randomCoord
  -- this will livelock if snake fills screen.
  if (c `elem` g ^. snake) then nextFood g else return c

randomCoord :: IO Coord
randomCoord = V2 <$> randomRIO (1, width) <*> randomRIO (1, height)

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  let g = Game
        { _snake  = (S.singleton (V2 10 10))
        , _dir    = North
        , _food   = (V2 0 0)
        , _score  = 0
        , _dead   = False
        , _paused = True
        , _frozen = False
        }
  nf <- nextFood g
  return $ g & food .~ nf

