import Data.Foldable
import Prelude hiding (foldr)

data Edge = Edge Char Trie deriving (Show)
data Trie = Node [Edge] deriving (Show)

root :: Trie
root = Node []

addPattern :: [Char] -> Trie -> Trie
addPattern [] trie = trie
addPattern (x:xs) (Node edges) =
  let ((Edge _ trie):rest) = asFirst edges x
  in Node $ Edge x (addPattern xs trie) : rest
  where asFirst [] char = [Edge char root]
        asFirst ((Edge x trie):xs) char = 
            if x == char
               then (Edge x trie):xs
               else asFirst xs char ++ [(Edge x trie)]

buildTrie :: [String] -> Trie
buildTrie = foldr addPattern root

data NumberedEdge = NumberedEdge Char NumberedTrie deriving (Show)
data NumberedTrie = NumberedNode Int [NumberedEdge] deriving (Show)

numberTrie :: Trie -> NumberedTrie
numberTrie trie =
  let (_, numberedTrie) = numberTrieFrom 0 trie
  in numberedTrie   
  where
    numberTrieFrom i (Node edges) = 
      let (total, numberedEdges) = foldr (numberAndAcc $ i + 1) (0, []) edges
      in (total, (NumberedNode i numberedEdges))
      where numberAndAcc i (Edge char trie) (acc, res) =
              let (num, numberedTrie) = numberTrieFrom (i + acc) trie
              in (num + acc + 1, res ++ [(NumberedEdge char numberedTrie)])

data LabeledEdge = LabeledEdge Char Int Int

instance Show LabeledEdge where
    show (LabeledEdge char x y) =
        (show x) ++ "->" ++ (show y) ++ ":" ++ [char]

labelTrie :: NumberedTrie -> [LabeledEdge]
labelTrie (NumberedNode i edges) = foldMap (buildLabel i) edges
  where buildLabel i (NumberedEdge char trie@(NumberedNode j _)) =
          (LabeledEdge char i j) : labelTrie trie

solve1 :: String -> String
solve1 input =
  let (_:patterns) = lines input
  in unlines $ map show . labelTrie . numberTrie $ buildTrie patterns

textMatchesPatterns :: String -> Trie -> Bool
textMatchesPatterns _ (Node []) = True
textMatchesPatterns [] (Node _) = False
textMatchesPatterns text (Node edges) = matchEdge text edges
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
  interact solve2
