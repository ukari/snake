{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module UI (main) where

import Control.Monad (forever, void, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Data.Functor (($>))

import System.Random (newStdGen, randomRs)

import Snake

import qualified Reflex as R
import qualified Reflex.Host.App as RH

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.ReflexMain (brickWrapper)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

-- Types

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake | Food | Empty

-- this forall is just a trick to "declare" 't' for signatures below.
main :: forall t . t ~ R.SpiderTimeline R.Global => IO ()
main = R.runSpiderHost $ RH.hostApp $ mdo

  (timerEvent, timerT) <- RH.newExternalEvent
  _                    <- RH.performPostBuild $ do
    void $ liftIO $ forkIO $ forever $ do
      _ <- timerT ()
      threadDelay 100000 -- decides how fast your game moves

  (eventE, finE, _suspendSetupF) <- brickWrapper shouldHaltE
                                                 widgetsDyn
                                                 cursorDyn
                                                 (pure theMap)

  -- tell ReflexHost to quit once the brickWrapper has shut down.
  RH.performPostBuild_ $ do
    pure $ RH.infoQuit $ pure finE

  let directionEvent = R.fforMaybe eventE $ (=<<) $ \case
        V.EvKey V.KUp         [] -> Just North
        V.EvKey V.KDown       [] -> Just South
        V.EvKey V.KRight      [] -> Just East
        V.EvKey V.KLeft       [] -> Just West
        V.EvKey (V.KChar 'k') [] -> Just North
        V.EvKey (V.KChar 'j') [] -> Just South
        V.EvKey (V.KChar 'l') [] -> Just East
        V.EvKey (V.KChar 'h') [] -> Just West
        _                        -> Nothing

  let restartEvent = R.fforMaybe eventE $ (=<<) $ \case
        V.EvKey (V.KChar 'r') [] -> Just ()
        _                        -> Nothing

  let cursorDyn = pure $ const Nothing -- never show cursor

  let shouldHaltE = R.fforMaybe eventE $ (=<<) $ \case
        V.EvKey V.KEsc        [] -> Just ()
        V.EvKey (V.KChar 'q') [] -> Just ()
        _                        -> Nothing

  outputDyn <- gameNetwork restartEvent directionEvent timerEvent

  let widgetsDyn = drawUI <$> outputDyn

  pure ()

-- Drawing

drawUI :: OutputState -> [Widget Name]
drawUI s = [C.center $ padRight (Pad 2) (drawStats s) <+> drawGrid s]

drawStats :: OutputState -> Widget Name
drawStats s = hLimit 11 $ vBox
  [drawScore (_out_score s), padTop (Pad 2) $ drawGameOver (_out_dead s)]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Score")
    $ C.hCenter
    $ padAll 1
    $ str
    $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver isDead = if isDead
  then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
  else emptyWidget

drawGrid :: OutputState -> Widget Name
drawGrid s =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Snake") $ vBox rows
 where
  rows = [ hBox $ cellsInRow r | r <- [height, height - 1 .. 1] ]
  cellsInRow y = [ drawCoord (V2 x y) | x <- [1 .. width] ]
  drawCoord = drawCell . cellAt
  cellAt c | c `elem` _out_snake s = Snake
           | c == _out_food s      = Food
           | otherwise             = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (snakeAttr   , V.blue `on` V.blue)
  , (foodAttr    , V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"
