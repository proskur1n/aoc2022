import Data.List (dropWhile, nub, sort)
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

countScannedFields :: [(Int, Int)] -> Int
countScannedFields ranges = go limits 0
  where
    nonEmptyRanges = filter (uncurry (<=)) ranges
    limits = sort $ concatMap (\(from, to) -> [Begin from, End to]) nonEmptyRanges
    go (Begin a : xs@(End b : _))   count = go xs (count + b - a + 1)
    go (Begin a : xs@(Begin b : _)) count = go xs (count + b - a)
    go (End a : xs@(Begin b : _))   count = go xs (count + b - a - 1)
    go (End a : xs@(End b : _))     count = go xs (count + b - a)
    go _ count = count

countFields :: [Region] -> Int
countFields regions = countScannedFields (map range regions) - length (nub $ map fst $ filter (\(_, y') -> y' == yLevel) beacons)
  where
    yLevel = 2000000
    beacons = map (\(_, b, _) -> b) regions -- May contain duplicates
    range ((x, y), _, d) = let d' = d - abs (y - yLevel) in (x - d', x + d')

interact' :: Show s => (String -> s) -> IO ()
interact' f = interact (\s -> show (f s) ++ "\n")

main :: IO ()
main = interact' (countFields . map parse . lines)
