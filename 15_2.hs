import Data.List (dropWhile, sort)
import Data.Char (isDigit)

type Sensor   = (Int, Int)
type Beacon   = (Int, Int)
type Distance = Int
type Region   = (Sensor, Beacon, Distance)

manhattan :: (Int, Int) -> (Int, Int) -> Distance
manhattan (x, y) (x', y') = abs (x' - x) + abs (y' - y)

numbers :: String -> [Int]
numbers line = case dropWhile (not . isNumber) line of
    "" -> []
    s  -> let (num, rest) = span isNumber s in read num : numbers rest
  where isNumber x = x == '-' || isDigit x

parse :: String -> Region
parse line = let [x, y, x', y'] = numbers line in ((x, y), (x', y'), manhattan (x, y) (x', y'))

data Limit = Begin Int | End Int deriving (Show)

instance Eq Limit where
    a == b = un a == un b
      where un (Begin x) = x ; un (End x) = x

instance Ord Limit where
    a <= b = un a <= un b
      where un (Begin x) = x ; un (End x) = x

scanRow :: [(Int, Int)] -> Maybe Int
scanRow ranges = go limits 0
  where
    nonEmptyRanges = filter (uncurry (<=)) ranges
    limits = sort $ concatMap (\(from, to) -> [Begin from, End to]) nonEmptyRanges
    go (End a : xs@(Begin b : _)) 1 = if a + 1 < b then Just (a + 1) else go xs 0
    go (Begin a : xs) d = go xs (d + 1)
    go (End a : xs) d = go xs (d - 1)
    go [] _ = Nothing

findDistressBeacon :: Int -> [Region] -> Beacon
findDistressBeacon 4000000 _ = error "distress beacon not found"
findDistressBeacon yLevel regions
    | Just x <- scanRow $ map range regions = (x, yLevel)
    | otherwise = findDistressBeacon (yLevel + 1) regions
  where
    range ((x, y), _, d) = let d' = d - abs (y - yLevel) in (x - d', x + d')

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' (findDistressBeacon 0 . map parse . lines)
