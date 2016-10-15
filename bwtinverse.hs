import Data.IntMap.Strict as IntMap hiding (map, toList)
import Data.Map.Strict as Map hiding (map, toList)
import Data.Array as Array

--import Test.QuickCheck
--import Test.QuickCheck.Gen

type Matrix = [String]

firstColumn :: Matrix -> String
firstColumn = map head

lastColumn :: Matrix -> String
lastColumn = map last

toMap :: [a] -> IntMap a
toMap text = IntMap.fromList $ zip [0..] text

buildIndexes :: String -> (Map Char Int, IntMap Int)
buildIndexes text = buildIndexesFrom 0 text Map.empty IntMap.empty
  where
    buildIndexesFrom _ [] countMap occIndex =
      let (_, startIndex) = Map.mapAccum accumfn 0 countMap
      in (startIndex, occIndex)
         
    buildIndexesFrom index (x:xs) countMap occIndex =
      let newCountMap = Map.insertWith (+) x 1 countMap
          newOccIndex = IntMap.insert index (newCountMap Map.! x) occIndex
      in buildIndexesFrom (index+1) xs newCountMap newOccIndex
         
    accumfn x y = let z = x+y in (z, x)

bwtinverse :: String -> String
bwtinverse bwt =
  let
    (fcstartind, lcoccind) = buildIndexes bwt
    lc = toMap bwt
  in bwtinverseStep 0 fcstartind lc lcoccind "$"
  where
    bwtinverseStep index fcstartind lc lcoccind text =
      let current = lc IntMap.! index :: Char
      in
        if current == '$'
        then text
        else
          bwtinverseStep (fcstartind Map.! current + lcoccind IntMap.! index - 1)
            fcstartind lc lcoccind $! (current:text)

main :: IO ()
main = do 
  interact $ unlines . map bwtinverse . lines

----- quichcheck

-- generatePermAt :: String -> Int -> String
-- generatePermAt text pos = drop pos text ++ take pos text

-- generatePerms :: String -> Matrix
-- generatePerms text = map (generatePermAt text) [0 .. length text - 1]

-- bwt :: String -> String
-- bwt = (map last) . sort . generatePerms 

-- mygen = resize 10000 $ listOf $ elements ['A', 'T', 'G', 'C']

-- --prop_bwtinverse :: String -> String -> Bool
-- prop_bwtinverse =
--   forAll mygen $ \gentext ->
--    let text = gentext ++ "$"
--    in text == (bwtinverse . bwt) text

-- check = quickCheckWith stdArgs { maxSuccess = 100 } prop_bwtinverse
