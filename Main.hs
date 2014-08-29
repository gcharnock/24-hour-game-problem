{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Prelude hiding (mapM, mapM_, elem)

import Control.Monad hiding (mapM, mapM_, forM, forM_, foldM)
import Data.Text hiding (zip,map,foldl,length,filter)
import Data.Monoid
import qualified Data.Set as S
import qualified Data.MultiMap as MM
import qualified Data.Map as M
import Data.Foldable

-- There is a game
-- N sessions of this game running at once
-- 5 sessions running over the 24 hour period

-- Information:
  --Perople they don't want to be with
  --Types of game they do not what to play

data GameType = Serious | Silly deriving (Ord, Eq, Show)

--If a game has a chracter type, then only that character type can join it
data Game = Game GameType CharacterType deriving (Eq, Ord, Show)

--Type of character
data CharacterType = Talky | Fighty | Neither deriving (Ord, Eq, Show)

data Player = Player {
  name :: Text,
  preferedCharacterType :: CharacterType,
  isHappyWithSilly :: Bool,
  dislikedPlayers :: S.Set Text
} deriving Show


------------------------------------------------------------
-- The actual solution 

doesPlayerFit :: Player -> Game -> Bool
doesPlayerFit player (Game gameType characterType) =
  case gameType of
    Serious -> True
    Silly -> isHappyWithSilly player
  &&
  case preferedCharacterType player of 
    Neither -> True
    ct -> ct == characterType


allocatePlayer :: Player -> [Game] -> MM.MultiMap Game Player -> [MM.MultiMap Game Player]
allocatePlayer player games allocation = do
  game <- filter (doesPlayerFit player) games
  
  let playerNamesAllreadyInGame = fmap name $ MM.lookup game allocation
      
  forM_ playerNamesAllreadyInGame $ \playerInGame ->
    guard (playerInGame `elem` dislikedPlayers player)
  
  return $ MM.insert game player allocation

solve :: [Player] -> [Game] -> [MM.MultiMap Game Player]
solve playerPool gameList = foldlM f MM.empty playerPool
  where f allocation player = allocatePlayer player gameList allocation


------------------------------------------------------------
-- Presenting results

printAllocation :: MM.MultiMap Game Player -> IO()
printAllocation allocation = 
  sequenceA_ $ M.mapWithKey f $ MM.toMap allocation
  where f g players = do putStrLn $ "    GAME:" <> show g
                         mapM_ (putStrLn.("        " <>).show) players

prettySolve :: [Player] -> [Game] -> IO ()
prettySolve a b = do
  let solutions = solve a b
  forM_ solutions $ \solution -> do 
    putStrLn "SOLUTION: "
    printAllocation solution
    
------------------------------------------------------------


--exampleSession :: Set Game
exampleSession :: [Game]
exampleSession = [Game Silly Fighty, 
                  Game Serious Neither, 
                  Game Serious Fighty, 
                  Game Serious Talky]

--There are about 30-35 players


trent :: Player
trent = Player {
    name = pack "trent",
    preferedCharacterType = Fighty,
    isHappyWithSilly = True,
    dislikedPlayers = S.fromList ["bob","eve"]
}

eve :: Player
eve = Player {
    name = pack "eve",
    preferedCharacterType = Talky,
    isHappyWithSilly = True,
    dislikedPlayers = S.fromList [""]
}

bob :: Player
bob = Player {
    name = pack "bob",
    preferedCharacterType = Neither,
    isHappyWithSilly = False,
    dislikedPlayers = S.fromList ["eve"]
}

------------------------------------------------------------

main :: IO ()
main = prettySolve [trent, eve, bob] exampleSession

