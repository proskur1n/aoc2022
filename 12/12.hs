import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), (><))
import Data.Maybe (fromJust)

type Pos = (Int, Int) -- (y, x)
type Height = Int
type Steps = Int

parse :: String -> (Map Pos Height, Pos, Pos)
parse input = (toHeight <$> grid, start, end)
  where
    grid =
      Map.fromDistinctAscList
        [ ((y, x), c)
        | (y, line) <- zip [1..] (lines input)
        , (x, c) <- zip [1..] line
        ]

    (start, _) = fromJust $ find ((== 'S') . snd) $ Map.assocs grid
    (end, _) = fromJust $ find ((== 'E') . snd) $ Map.assocs grid

    toHeight 'S' = toHeight 'a'
    toHeight 'E' = toHeight 'z'
    toHeight c = fromEnum c - fromEnum 'a'

bfs :: (Pos -> Pos -> Bool) -> (Pos -> Bool) -> Pos -> Maybe Steps
bfs isWalkable isEnd start = go Set.empty (Seq.singleton (start, 0))
  where
    go visited queue =
      case Seq.viewl queue of
        EmptyL ->
          Nothing
        (pos, steps) :< queue' ->
          if isEnd pos then
            Just steps
          else if pos `Set.member` visited then
            go visited queue'
          else
            go (Set.insert pos visited) (queue' >< Seq.fromList (map (, steps + 1) $ getNeighbors pos))

    getNeighbors pos@(y, x) =
      filter (isWalkable pos) [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

partOne :: Map Pos Height -> Pos -> Pos -> Steps
partOne heightmap start end = fromJust $ bfs isWalkable (== end) start
  where
    isWalkable pos neighbor =
      neighbor `Map.member` heightmap && heightmap!neighbor <= heightmap!pos + 1

partTwo :: Map Pos Height -> Pos -> Steps
partTwo heightmap end = fromJust $ bfs isWalkable (\pos -> heightmap!pos == 0) end
  where
    isWalkable pos neighbor =
      neighbor `Map.member` heightmap && heightmap!neighbor >= heightmap!pos - 1

main :: IO ()
main = do
  (heightmap, start, end) <- parse <$> readFile "input"
  putStrLn $ "Part One: " ++ show (partOne heightmap start end)
  putStrLn $ "Part Two: " ++ show (partTwo heightmap end)
