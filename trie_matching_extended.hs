import Data.Foldable
import Prelude hiding (foldr)

data Edge = Edge Char Trie deriving (Show)
data Trie = Node Bool [Edge] deriving (Show)

root :: Trie
root = Node False []

addPattern :: [Char] -> Trie -> Trie
addPattern [] (Node _ edges) = (Node True edges)
addPattern (x:xs) (Node matches edges) =
  let ((Edge _ trie):rest) = asFirst edges x
  in Node matches $ Edge x (addPattern xs trie) : rest
  where asFirst [] char = [Edge char (Node False [])]
        asFirst ((Edge x trie):xs) char = 
            if x == char
               then (Edge x trie):xs
               else asFirst xs char ++ [(Edge x trie)]

buildTrie :: [String] -> Trie
buildTrie = foldr addPattern root

textMatchesPatterns :: String -> Trie -> Bool
textMatchesPatterns [] (Node matches _) = matches
textMatchesPatterns text (Node True _) = True
textMatchesPatterns text (Node _ edges) = matchEdge text edges
  where
    matchEdge text [] = False
    matchEdge text@(x:xs) ((Edge c trie):es)
      | x == c = textMatchesPatterns xs trie
      | x /= c = matchEdge text es

positionsForMatches :: String -> Trie -> [Int]
positionsForMatches text trie = [i | i <- [0..length text - 1],
                                     textMatchesPatterns (drop i text) trie]

solve2 :: String -> String
solve2 input =
  let (text:_:patterns) = lines input
  in unwords . map show . positionsForMatches text $ buildTrie patterns

main :: IO ()
main = do
  --interact id
  interact solve2
