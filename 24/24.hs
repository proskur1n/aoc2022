import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int) -- (y, x)
type Vel = (Int, Int)
type Time = Int

data Valley
  = Valley
  { blizzards :: [(Pos, Vel)]
  , width :: Int
  , height :: Int
  } deriving (Eq, Show)

parse :: String -> Valley
parse input =
  Valley
    { blizzards = mapMaybe toBlizzard chars
    , width = width
    , height = height
    }
  where
    rows = lines input
    height = length rows
    width = length $ head rows
    chars = [((y, x), c) | (y, row) <- zip [1..] rows, (x, c) <- zip [1..] row]

    toBlizzard (pos, '^') = Just (pos, (-1, 0))
    toBlizzard (pos, 'v') = Just (pos, (1, 0))
    toBlizzard (pos, '<') = Just (pos, (0, -1))
    toBlizzard (pos, '>') = Just (pos, (0, 1))
    toBlizzard _ = Nothing

cacheBlizzardsOverTime :: Valley -> Set (Time, Pos)
cacheBlizzardsOverTime Valley {blizzards, width, height} =
  Set.fromList
    [ (time, pos)
    | (time, blizzardsAtTime) <- zip [0..modulo-1] $ iterate (map timestep) blizzards
    , (pos, _) <- blizzardsAtTime
    ]
  where
    modulo =
      lcm (width - 2) (height - 2)

    timestep ((y, x), vel@(dy, dx)) = ((y', x'), vel)
      where
        x' = 2 + (x + dx - 2) `mod` (width - 2)
        y' = 2 + (y + dy - 2) `mod` (height - 2)

bfs :: Valley -> Pos -> Pos -> Time -> Time
bfs valley =
  let
    blizzardsOverTime = cacheBlizzardsOverTime valley
    modulo = lcm (width valley - 2) (height valley - 2)
  in
    \start finish time0 ->
      let
        go visited ((time, pos@(y, x)) :<| queue)
            | pos == finish = time - time0
            | (time `mod` modulo, pos) `Set.member` visited = go visited queue
            | otherwise = go visited' (queue >< Seq.fromList neighbors)
            where
              visited' = Set.insert (time `mod` modulo, pos) visited
              neighbors = (time + 1, ) <$> filter validPosition [(y, x), (y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

        validPosition pos@(y, x) =
          pos == start || pos == finish || (1 < x && x < width valley && 1 < y && y < height valley)
      in
        go blizzardsOverTime (Seq.singleton (time0, start))

main :: IO ()
main = do
  valley <- parse <$> readFile "input"
  let bfs' = bfs valley
  let entrance = (1, 2)
  let exit = (height valley, width valley - 1)
  let entranceToExit = bfs' entrance exit 0
  let exitToEntrance = bfs' exit entrance entranceToExit
  let entranceToExit2 = bfs' entrance exit (entranceToExit + exitToEntrance)
  putStrLn $ "Part One: " ++ show entranceToExit
  putStrLn $ "Part Two: " ++ show (entranceToExit + exitToEntrance + entranceToExit2)
