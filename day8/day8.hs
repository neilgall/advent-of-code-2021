{-# LANGUAGE OverloadedStrings #-}

-- Unfinished; reverted to CLP programming in Prolog instead

import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Exit


type Text = T.Text

-- model

data Entry = Entry [Text] [Text]
  deriving (Show)


-- parsing

entry :: Text -> Entry
entry s = Entry patterns digits
  where
    [pstr, dstr] = T.splitOn " | " s
    patterns = T.words pstr
    digits = T.words dstr

entries :: Text -> [Entry]
entries = map entry . T.lines


-- part 1

is1478 :: Text -> Bool
is1478 s = (T.length s) `elem` [2, 3, 4, 7]

part1 :: [Entry] -> Int
part1 = 
  length . filter is1478 . concat . map (\(Entry _ digits) -> digits)


-- part 2

type Constraint = S.Set Char
type Constraints = M.Map Char Constraint

unconstrained :: Constraint
unconstrained = S.fromList ['0'..'9']

newConstraints :: Constraints
newConstraints = 
  M.fromList $ map (\k -> (k, unconstrained)) "abcdefg"

constrain :: Constraint -> Constraints -> Char -> Constraints
constrain cs m c = M.update constrain' c m
  where
    constrain' cs' = Just $ S.intersection cs cs' 

buildMapping :: [Text] -> Constraints
buildMapping patterns = 
  foldl addPattern newConstraints sortedPatterns
  where
    sortedPatterns = sortWith T.length patterns

sortWith :: Ord a => (t -> a) -> [t] -> [t]
sortWith f ts = L.sortBy (\a b -> compare (f a) (f b)) ts

addPattern :: Constraints -> Text -> Constraints
addPattern map pattern = 
  foldl constrain' map (T.unpack pattern)
  where
    constrain' = constrain (candidates pattern)

candidates :: Text -> Constraint
candidates pattern = 
  S.fromList $ case (T.length pattern) of
    2 -> "1"
    3 -> "7"
    4 -> "4"
    5 -> "235"
    6 -> "069"
    7 -> "8"


evalDigit :: Constraints -> Text -> Char
evalDigit map digits =
  S.elemAt 0 $ foldl constrain' unconstrained (T.unpack digits)
  where
    constrain' :: Constraint -> Char -> Constraint
    constrain' cs d = S.intersection cs (lookup' d)
    lookup' d = fromJust $ M.lookup d map

part2 :: Entry -> Int
part2 (Entry patterns digits) = 
  evalDigits (buildMapping patterns) digits
  where
    evalDigits m ds = read $ map (evalDigit m) ds


-- tests

assert :: Bool -> String -> IO String
assert expr info = 
  if expr then (return info) else die ("fail: " ++ info)

part1_tests entries = do
    assert (part1 entries == 26) "part1"

part2_tests entries = do
    assert (part2 (entries !! 0) == 8394) "part2.0"
    assert (part2 (entries !! 1) == 9781) "part2.1"
    assert (part2 (entries !! 2) == 1197) "part2.2"
    assert (part2 (entries !! 3) == 9361) "part2.3"
    assert (part2 (entries !! 4) == 4873) "part2.4"
    assert (part2 (entries !! 5) == 8418) "part2.5"
    assert (part2 (entries !! 6) == 4548) "part2.6"
    assert (part2 (entries !! 7) == 1625) "part2.7"
    assert (part2 (entries !! 8) == 8717) "part2.8"
    assert (part2 (entries !! 9) == 4315) "part2.9"

tests = do
  entries <- fmap entries $ TIO.readFile "example.txt"
  part1_tests entries
  part2_tests entries

main = do
  tests
  entries <- fmap entries $ TIO.readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 entries)
