{-# LANGUAGE BlockArguments #-}

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Control.Applicative (asum)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int) -- (y, x)
data Dir = N | S | W | E deriving (Eq, Ord)

parse :: String -> [Pos]
parse input = do
  (y, row) <- zip [1..] (lines input)
  (x, char) <- zip [1..] row
  if char == '#' then pure (y, x) else []

singleRound :: [Pos] -> [Dir] -> ([Pos], Int)
singleRound elves dirs =
  let
    occupied = Set.fromList elves
    proposed = map (\elf -> propose elf occupied dirs) elves
    proposedTimes = Map.fromListWith (+) $ mapMaybe ((, 1) <$>) proposed
    updatedElves =
      zipWith (
        \oldPos proposition ->
          case proposition of
            Just newPos ->
              if proposedTimes!newPos > 1 then
                oldPos
              else
                newPos
            Nothing ->
              oldPos
      ) elves proposed
  in
    (updatedElves, length $ filter (uncurry (/=)) $ zip elves updatedElves)
  where
    propose :: Pos -> Set Pos -> [Dir] -> Maybe Pos
    propose (y, x) elves dirs =
      let
        neighbors = [(y', x') | y' <- [y-1..y + 1], x' <- [x-1..x+1], (y', x') /= (y, x)]
      in
        if all (`Set.notMember` elves) neighbors then
          Nothing
        else
          asum $ map (
            \dir ->
              let
                adjacent =
                  case dir of
                    N -> [(y - 1, x), (y - 1, x - 1), (y - 1, x + 1)]
                    S -> [(y + 1, x), (y + 1, x - 1), (y + 1, x + 1)]
                    W -> [(y, x - 1), (y - 1, x - 1), (y + 1, x - 1)]
                    E -> [(y, x + 1), (y - 1, x + 1), (y + 1, x + 1)]
              in
                if all (`Set.notMember` elves) adjacent then
                  Just $ head adjacent
                else
                  Nothing
          ) dirs

simulate :: [Pos] -> [([Pos], Int)]
simulate elves = scanl (singleRound . fst) (elves, 0) $ iterate shift [N, S, W, E]
  where
    shift :: [a] -> [a]
    shift (x:xs) = xs ++ [x]
    shift [] = []

partOne :: [Pos] -> Int
partOne input = emptyTiles $ fst $ simulate input !! 10
  where
    emptyTiles :: [Pos] -> Int
    emptyTiles elves =
      let
        x0 = minimum $ map snd elves
        x1 = maximum $ map snd elves
        y0 = minimum $ map fst elves
        y1 = maximum $ map fst elves
      in
        (x1 - x0 + 1) * (y1 - y0 + 1) - length elves

partTwo :: [Pos] -> Int
partTwo input = 1 + length (takeWhile (\(_, moved) -> moved > 0) $ tail $ simulate input)

main :: IO ()
main = do
  elves <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne elves)
  putStrLn $ "Part Two: " ++ show (partTwo elves)
