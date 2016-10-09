import Data.Foldable hiding (toList)
import Data.Array
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

buildTrie :: Int -> Array Int Char -> Trie
buildTrie size array = 
    foldr addArraySuffix root $ generateSuffixes size array
    where
      addArraySuffix (start, text) trie = addSuffix text start trie

generateSuffixes :: Int -> Array Int Char -> [(Int, [Char])]
generateSuffixes size
    array = map (subArray array) [0 .. size - 1]
    where 
      subArray array start = (start, elems $ slice start (size - start) array)

data TreeEdge = TreeEdge Char Tree | MergedTreeEdge Int Int Tree deriving (Show)
data Tree = TreeNode [TreeEdge] | TreeLeaf Int deriving (Show)

rootTree :: Tree
rootTree = TreeNode []

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

addSuffixToTree :: Int -> Array Int Char -> [Char] -> Tree -> Tree
addSuffixToTree start array text = addSuffixToTreeFrom start array text start
    where 
      addSuffixToTreeFrom start array text pos (TreeNode edges) =                  
          (TreeNode $ mergeEdges pos text array start edges)

      mergeEdges pos text _ start [] = 
          [buildEdge pos text (TreeLeaf start)]
      mergeEdges pos text@(x:xs) array start (edge:rest) =
          case edge of
            (TreeEdge char tree) ->
                if char == x 
                then (TreeEdge char $ addSuffixToTreeFrom start array xs (pos+1) tree) : rest
                else edge : mergeEdges pos text array start rest
            (MergedTreeEdge subtextStart length tree) ->
                if array!subtextStart == x
                then splitMergedEdge pos text array start edge : rest
                else edge : mergeEdges pos text array start rest

      buildEdge pos (x:[]) tree = (TreeEdge x tree)
      buildEdge pos text tree = (MergedTreeEdge pos (length text) $ tree)

      splitMergedEdge pos text array start edge@(MergedTreeEdge subtextStart size tree) =
          let subtext = elems $ slice subtextStart size array
              commonSuffix = extractCommon subtext text
              commonLength = length commonSuffix
              restNew = drop commonLength text
              restExisting = drop commonLength subtext
          in 
            buildEdge pos commonSuffix 
                          $ addSuffixToTreeFrom start array restNew (commonLength + pos)
                          $ addSuffixToTreeFrom start array restExisting (commonLength + subtextStart) tree

extractCommon :: String -> String -> String
extractCommon [] _ = []
extractCommon _ [] = []
extractCommon (x:xs) (e:es) = 
    if x == e 
    then x : extractCommon xs es
    else []

buildTreeFromScratch :: Int -> Array Int Char -> Tree
buildTreeFromScratch size array = 
    foldr addArraySuffix rootTree $ generateSuffixes size array
    where
      addArraySuffix (start, text) trie = addSuffixToTree start array text trie


slice :: Int -> Int -> Array Int Char -> Array Int Char
slice start length array = ixmap (0, length-1) ((+) start) array

collectEdges :: Array Int Char -> Tree -> [String]
collectEdges _ (TreeLeaf _) = []
collectEdges text (TreeNode edges) = foldMap (collectEdgesFromEdge text) edges
  where collectEdgesFromEdge text (TreeEdge char tree) =
          [char] : collectEdges text tree
        collectEdgesFromEdge text (MergedTreeEdge start length tree) =
          (elems $ slice start length text) : collectEdges text tree
  
solve4 :: String -> String
solve4 input =
  let (text:_) = lines input
      size = length text
      array = (listArray (0, size  - 1) text)
  in unlines . collectEdges array $ buildTreeFromScratch size array

main :: IO ()
main = do
  interact solve4
