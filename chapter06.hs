
import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) $ tails haystack

encode :: Int -> String -> String
encode offset = map (chr . (+ offset) . ord)

decode :: Int -> String -> String
decode offset = map (chr . (subtract offset) . ord)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

firstTo40' :: Maybe Int
firstTo40' = firstTo 40

phoneBook =
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k,v) -> k == key)

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k, v) : xs)
  | k == key = Just v
  | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

phoneBook' :: Map.Map String String
phoneBook' = Map.fromList $
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ]

newBook = Map.insert "grace" "341-9021" phoneBook'

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

intBook = Map.map string2digits phoneBook'

phoneBook'' =
  [("betty", "555-2938")
  ,("betty", "342-2492")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("patsy", "943-2929")
  ,("patsy", "827-9162")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ,("penny", "555-2111")
  ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs


sphereVolume :: Float -> Float
sphereVolume = Sphere.volume

sphereArea :: Float -> Float
sphereArea = Sphere.area

cubeVolume :: Float -> Float
cubeVolume = Cube.volume

cubeArea :: Float -> Float
cubeArea = Cube.area

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume = Cuboid.volume

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea = Cuboid.area

