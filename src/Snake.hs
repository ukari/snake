{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Functor (($>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad (forever, void)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (newStdGen, randomRs, randomRIO)

import Control.Concurrent (threadDelay, forkIO)

import qualified Reflex as R
import qualified Reflex.Host.App as RH

-- General Utility

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- Types

data OutputState = OutputState
  { _out_dead :: Bool
  , _out_score :: Int
  , _out_snake :: Seq Coord
  , _out_food :: Coord
  }

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


-- Game core network

gameNetwork
  :: forall t m
   . ( t ~ R.SpiderTimeline R.Global
     , MonadFix m
     , R.MonadHold t m
     , RH.MonadAppHost t m
     )
  => R.Event t ()
  -> R.Event t Direction
  -> R.Event t ()
  -> m (R.Dynamic t OutputState)
gameNetwork restartEvent directionEvent tickEvent = mdo
  -- the state changes affect each other, so we use recursive do here.

  startEvent                 <- R.headE tickEvent

  pause :: R.Behavior t Bool <- R.hold True
    $ R.mergeWith (||) [restartEvent $> True, directionEvent $> False]

  dead :: R.Dynamic t Bool <- R.holdDyn False $ R.leftmost
    [ restartEvent $> False
    , R.tag (snakeDiesOnMove <$> R.current nextHeadDyn <*> R.current snakeDyn)
            tickEvent
    ]

  let moveEvent = R.attachWithMaybe
        (\paused dying -> [ () | not (dying || paused) ])
        pause
        (R.tagPromptlyDyn dead tickEvent) -- need promptly to prevent tick
                                         -- if dead in the same instant.

  snakeDyn :: R.Dynamic t (Seq Coord) <- snakeNetwork startEvent
                                                      moveEvent
                                                      (R.current nextHeadDyn)
                                                      (R.current foodDyn)

  nextHeadDyn :: R.Dynamic t (Maybe Coord) <- snakeHeadNetwork moveEvent
                                                               dead
                                                               snakeDyn

  foodDyn :: R.Dynamic t Coord <- foodNetwork startEvent snakeDyn

  scoreDyn :: R.Dynamic t Int  <- scoreNetwork (R.current foodDyn) snakeDyn

  pure $ OutputState <$> dead <*> scoreDyn <*> snakeDyn <*> foodDyn
 where

  foodNetwork startEvent snakeDyn = do
    let genNewFoodM fs = R.sample (R.current snakeDyn) <&> genNewFood fs
        genNewFood fs snake = dropWhile (`elem`snake) fs
    (foodDecayEvent, decayT) <- RH.newExternalEvent
    _                        <- RH.performPostBuild $ do
      void $ liftIO $ forkIO $ forever $ do
        _ <- decayT ()
        threadDelay =<< randomRIO (3000000, 10000000)
    let foodChange = R.leftmost
          [ startEvent $> \fs -> genNewFoodM fs
          , restartEvent $> \fs -> genNewFoodM fs
          , R.updated snakeDyn <&> \snake fs -> pure (genNewFood fs snake)
          , foodDecayEvent $> \fs -> genNewFoodM $ tail fs
          ]
    infiniteFoodSupply <- liftIO
      [ zipWith V2 x y
      | x <- newStdGen <&> randomRs (1, width)
      , y <- newStdGen <&> randomRs (1, height)
      ]
    allTheFood :: R.Dynamic t [Coord] <- R.foldDynM id
                                                    infiniteFoodSupply
                                                    foodChange
    pure $ head <$> allTheFood

  snakeHeadNetwork moveEvent dead snakeDyn = mdo
    lastDirDyn :: R.Dynamic t Direction <- R.holdDyn NoDir $ R.leftmost
      [restartEvent $> NoDir, R.tag (R.current nextDirDyn) moveEvent]
    nextDirDyn :: R.Dynamic t Direction <-
      R.holdDyn NoDir $ R.gate (not <$> R.current dead) $ R.attachWithMaybe
        turnDir
        (R.current lastDirDyn)
        directionEvent
    pure $ calcNextHead <$> nextDirDyn <*> snakeDyn

  scoreNetwork foodB snakeDyn = do
    let scoreChange = R.leftmost
          [ restartEvent $> const 0
          , R.attachWith
            (\food g -> if getSnakeHead g == food then (+10) else id)
            foodB
            (R.updated snakeDyn)
          ]
    R.foldDyn id 0 scoreChange

  snakeNetwork startEvent moveEvent nextHeadB foodB = do
    let snakeChangeE :: R.Event t (Snake -> Snake) = R.mergeWith
          (.)
          [ R.attachWith
            id
            (   (\nextHead food () snake -> eatOrMove nextHead food snake)
            <$> nextHeadB
            <*> foodB
            )
            moveEvent
          , restartEvent <&> \() _ -> initialSnake
          , startEvent $> id -- ensures we render the initial screen
          ]

    R.foldDyn id initialSnake snakeChangeE
