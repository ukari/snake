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
  { _snake   :: Snake     -- ^ snake as a sequence of points in R2
  , _lastdir :: Direction -- ^ direction
  , _nextdir :: Direction
  , _food    :: Coord     -- ^ location of the food
  , _dead    :: Bool      -- ^ game over flag
  , _score   :: Int       -- ^ score
  } deriving (Eq, Show)

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  | NoDir
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
  guard (not $ g ^. dead)
  die g <|> eatFood g <|> move g

-- | Possibly die if next head position is disallowed
die :: Monad m => Game -> Maybe (m Game)
die g = [ return $ g & dead .~ True | bodyHit || borderHit ]
 where
  bodyHit   = nextHead g `elem` g ^. snake
  borderHit = outOfBounds (nextHead g)

-- | Possibly eat food if next head position is food
eatFood :: Game -> Maybe (IO Game)
eatFood g =
  [ do
      let ng = g & score %~ (+10) & snake %~ (nextHead g<|)
      nf <- nextFood ng
      return $ ng & food .~ nf
  | nextHead g == g ^. food
  ]

-- | Move snake along in a marquee fashion
move :: Monad m => Game -> Maybe (m Game)
move g =
  Just $ return $ g & snake %~ (mv . S.viewr) & lastdir .~ (g ^. nextdir)
 where
  mv (EmptyR) = error "Snakes can't be empty!"
  mv (s:>_  ) = nextHead g <| s

-- | Get next head location of the game's snake
nextHead :: Game -> Coord
nextHead g = go $ S.viewl (g ^. snake) -- nextHead (g ^. dir) (g ^. snake)
 where
  go (EmptyL) = error "Snakes can't be empty!"
  go (a:<_  ) = case g ^. nextdir of
    North -> a & _y %~ (+1)
    South -> a & _y %~ (subtract 1)
    East  -> a & _x %~ (+1)
    West  -> a & _x %~ (subtract 1)
    NoDir -> a

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game
turn d g = case turnDir d (g ^. lastdir) of
  Nothing     -> g
  Just newdir -> g & nextdir .~ newdir

turnDir :: Direction -> Direction -> Maybe Direction
turnDir n c | n == opposite c = Nothing
            | otherwise       = Just n
 where
  opposite North = South
  opposite South = North
  opposite East  = West
  opposite West  = East
  opposite NoDir = NoDir

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
        { _snake   = (S.singleton (V2 10 10))
        , _lastdir = NoDir
        , _nextdir = NoDir
        , _food    = (V2 0 0)
        , _score   = 0
        , _dead    = False
        }
  nf <- nextFood g
  return $ g & food .~ nf

