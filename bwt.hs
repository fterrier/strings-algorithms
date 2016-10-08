import Data.List

type Matrix = [String]

generatePermAt :: String -> Int -> String
generatePermAt text pos = drop pos text ++ take pos text

generatePerms :: String -> Matrix
generatePerms text = map (generatePermAt text) [0 .. length text - 1]

bwt :: String -> String
bwt = (map last) . sort . generatePerms 

main :: IO ()
main = do 
  interact $ unlines . map bwt . lines
