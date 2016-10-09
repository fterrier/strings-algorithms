import Data.Foldable hiding (toList)
import Data.Vector (Vector, slice, fromList, toList)
import Prelude hiding (foldr)

data Edge = Edge Char Trie deriving (Show)
data Trie = Node [Edge] | Leaf Int deriving (Show)

root :: Trie
root = Node []

addSuffix :: [Char] -> Int -> Trie -> Trie
addSuffix [] start trie = (Leaf start)
addSuffix (x:xs) start (Node edges) =
  let ((Edge _ trie):rest) = asFirst edges x
  in Node $ Edge x (addSuffix xs start trie) : rest
  where asFirst [] char = [Edge char (Node [])]
        asFirst ((Edge x trie):xs) char = 
            if x == char
               then (Edge x trie):xs
               else asFirst xs char ++ [(Edge x trie)]

buildTrie :: [Char] -> Trie
buildTrie text = foldr (\(start, text) trie -> addSuffix text start trie) root
  $ map (\x -> (x, (flip drop text x))) [0 .. length text - 1]

data TreeEdge = TreeEdge Char Tree | MergedTreeEdge Int Int Tree deriving (Show)
data Tree = TreeNode [TreeEdge] | TreeLeaf Int deriving (Show)

buildTree :: Trie -> Tree
buildTree = snd . buildTreeFrom 0
  where buildTreeFrom _ (Leaf start) = (start, (TreeLeaf start))
        buildTreeFrom pos (Node edges) =
          let edgesTuple = map buildEdge edges
              (start:_) = map fst edgesTuple
          in (start, (TreeNode $ map snd edgesTuple))
          where
            buildEdge edge@(Edge char _) =
              let (mergeLength, start, tree) = mergeToNextBranch 1 pos edge
              in if mergeLength > 1
                    then (start, (MergedTreeEdge (start + pos) mergeLength tree))
                    else (start, (TreeEdge char tree))
                         
            mergeToNextBranch length pos (Edge _ (Node (x:[]))) =
              mergeToNextBranch (length + 1) (pos + 1) x
            mergeToNextBranch length pos (Edge char trie) =
              let (start, tree) = buildTreeFrom (pos + 1) trie
              in (length, start, tree)
            
collectEdges :: Vector Char -> Tree -> [String]
collectEdges _ (TreeLeaf _) = []
collectEdges text (TreeNode edges) = foldMap (collectEdgesFromEdge text) edges
  where collectEdgesFromEdge text (TreeEdge char tree) =
          [char] : collectEdges text tree
        collectEdgesFromEdge text (MergedTreeEdge start length tree) =
          (toList $ slice start length text) : collectEdges text tree
  
solve4 :: String -> String
solve4 input =
  let (text:_) = lines input
  in unlines . collectEdges (fromList text) $ buildTree $ buildTrie text

main :: IO ()
main = do
  interact solve4
