import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List (sort)
import Data.Foldable (maximumBy)
import Data.Ord (Ord, compare)
import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Combinator (many1)
import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Either (rights)

main = do
  ls <- lines <$> readFile "input-4.txt"
  let actions = rights $ fmap parseLine ls
  let shifts = guardShifts actions
  let shiftMap = fmap concat $ collectEntries shifts
  let byMostSleepingMinute = fmap minuteInIntervalsWhenMostLikelySleeping shiftMap

  let sleepiestGuardID = mostSleepingGuard shiftMap
  let part1 = sleepiestGuardID * (byMostSleepingMinute ! sleepiestGuardID)
  
  let consistentlySleepyGuardID = consistentlySleepyGuard shiftMap
  let part2 = consistentlySleepyGuardID * (byMostSleepingMinute ! consistentlySleepyGuardID)
  
  return (part1, part2)

-- parsing 
parseLine = parse action ""

action :: Parser GuardAction
action = do
  ts <- timestamp
  void $ char ' '
  act <- actionType ts
  return act

actionType :: Timestamp -> Parser GuardAction
actionType ts = beginShiftP ts <|> fallAsleepP ts <|> wakeUpP ts

beginShiftP :: Timestamp -> Parser GuardAction
beginShiftP ts = do
  void $ string "Guard #"
  guardID <- num
  void $ string " begins shift"
  return $ BeginShift guardID ts

fallAsleepP :: Timestamp -> Parser GuardAction
fallAsleepP ts = do
  void $ string "falls asleep"
  return $ FallAsleep ts

wakeUpP :: Timestamp -> Parser GuardAction
wakeUpP ts = do
  void $ string "wakes up"
  return $ WakeUp ts

timestamp :: Parser Timestamp
timestamp = do
  void $ char '['
  year <- num
  void $ char '-'
  month <- num
  void $ char '-'
  date <- num
  void $ char ' '
  hour <- num
  void $ char ':'
  minute <- num
  void $ char ']'
  return Timestamp { year = year, month = month , date = date, hour = hour, minute = minute }

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

-- timestamps
data Timestamp = Timestamp { year :: Int
                           , month :: Int
                           , date :: Int
                           , hour :: Int
                           , minute :: Int 
                           } deriving (Show, Eq)

instance Ord Timestamp where
  ((<=)) l r = minutes l <= minutes r

minutes :: Timestamp -> Int
minutes (Timestamp { year = y, month = o, date = d, hour = h, minute = m }) =
  m + (h * 60) + (d * 60 * 24) + (o * 60 * 24 * 31) + (y * 60 * 24 * 366)

emptyTS = Timestamp { }

-- intervals
type Interval = (Timestamp, Timestamp)                           
startInterval ts = (ts, emptyTS)
endInterval (start, _) ts = (start, ts)

totalMinutesSleeping :: [Interval] -> Int
totalMinutesSleeping = sum . (fmap intervalMinutes)

intervalMinutes :: Interval -> Int
intervalMinutes (start, end) = minutes end - minutes start

minuteInIntervalsWhenMostLikelySleeping :: [Interval] -> Int
minuteInIntervalsWhenMostLikelySleeping = 
  fst . minuteSleepingMostOften . concat . fmap minutesSleeping

consistentSleeping :: [Interval] -> Int
consistentSleeping [] = 0
consistentSleeping xs =
  (snd . minuteSleepingMostOften . concat . fmap minutesSleeping) xs

minutesSleeping :: Interval -> [Int]
minutesSleeping (start, end) = [minute start .. (minute end) - 1]

minuteSleepingMostOften :: [Int] -> (Int, Int)
minuteSleepingMostOften = entryForMaxValue . mapCount

-- guard actions
type GuardID = Int
data GuardAction = BeginShift GuardID Timestamp
                 | FallAsleep Timestamp
                 | WakeUp Timestamp
                 deriving (Show, Eq)
instance Ord GuardAction where
  ((<=)) l r = timeOf l <= timeOf r                 

timeOf :: GuardAction -> Timestamp
timeOf (BeginShift _ ts) = ts
timeOf (FallAsleep ts) = ts
timeOf (WakeUp ts) = ts

guardShifts :: [GuardAction] -> [GuardShift]
guardShifts = (foldl addActionToShift []) . sort

addActionToShift :: [GuardShift] -> GuardAction -> [GuardShift]
addActionToShift shifts action =
  case (action, shifts) of
    (BeginShift id _, _) -> (id, []) : shifts -- todo: do i need to capture ts?
    (FallAsleep ts, (shift : restShifts)) -> (fallAsleep ts shift) : restShifts
    (WakeUp ts, (shift : restShifts)) -> (wakeUp ts shift) : restShifts

-- guard shifts
type GuardShift = (GuardID, [Interval])
type GuardShiftMap = Map.Map GuardID [Interval]

fallAsleep :: Timestamp -> GuardShift -> GuardShift
fallAsleep ts (id, intervals) =
  (id, startInterval ts : intervals)

wakeUp :: Timestamp -> GuardShift -> GuardShift
wakeUp ts (id, lastInterval : restIntervals) = 
  (id, endInterval lastInterval ts : restIntervals)

mostSleepingGuard :: GuardShiftMap -> GuardID
mostSleepingGuard = keyForMaxValue . (fmap totalMinutesSleeping)

consistentlySleepyGuard :: GuardShiftMap -> GuardID
consistentlySleepyGuard = keyForMaxValue . (fmap consistentSleeping)

-- utils
mapCount :: Ord t => [t] -> Map t Int
mapCount = foldr (\key -> Map.insertWith (+) key 1) Map.empty

entryForMaxValue :: Map t Int -> (t, Int)
entryForMaxValue mp = maximumBy sortPairByValue $ Map.toList mp
sortPairByValue (_, l) (_, r) = compare l r

keyForMaxValue :: Map t Int -> t
keyForMaxValue = fst . entryForMaxValue

collectEntries :: Ord k => [(k, v)] -> Map k [v]
collectEntries = 
  foldr (\(key, value) -> Map.insertWith (++) key [value]) Map.empty
