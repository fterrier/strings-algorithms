import Data.Foldable hiding (toList)
import Prelude hiding (foldr, foldl)

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
buildTrie array = 
    foldr addArraySuffix root $ generateSuffixes 0 array
    where
      addArraySuffix (start, text) trie = addSuffix text start trie

generateSuffixes :: Int -> [Char] -> [(Int, [Char])]
generateSuffixes length text@(x:xs) = (length,text) : generateSuffixes (length+1) xs
generateSuffixes _ [] = []

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

addSuffixToTree :: Int -> [Char] -> [Char] -> Tree -> Tree
addSuffixToTree start ft text = addSuffixToTreeFrom start ft text start
    where
      addSuffixToTreeFrom start _ [] pos tree = tree
      addSuffixToTreeFrom start ft text pos (TreeNode edges) =                  
        (TreeNode $ mergeEdges pos text ft start edges)

      mergeEdges pos text ft start [] = 
          [buildEdge pos text (TreeLeaf start)]
      mergeEdges pos text@(x:xs) ft start (edge:rest) =
          case edge of
            (TreeEdge char tree) ->
              if char == x 
              then (TreeEdge char $ addSuffixToTreeFrom start ft xs (pos+1) tree) : rest
              else edge : mergeEdges pos text ft start rest
            (MergedTreeEdge subtextStart length tree) ->
              let subtext = slice subtextStart length ft
                  (commonSuffix, restExisting, restNew) = extractCommon subtext text
              in if commonSuffix /= ""
                 then splitMergedEdge pos text ft start edge
                      commonSuffix restExisting restNew : rest
                 else edge : mergeEdges pos text ft start rest

      buildEdge pos (x:[]) tree = (TreeEdge x tree)
      buildEdge pos text tree = (MergedTreeEdge pos (length text) $ tree)

      splitMergedEdge pos text ft start edge@(MergedTreeEdge subtextStart size tree)
          commonSuffix restExisting restNew =
        if length commonSuffix == size
        then
          buildEdge pos commonSuffix
          $ addSuffixToTreeFrom start ft restNew (length commonSuffix + pos) tree
        else
          buildEdge pos commonSuffix
          $ addSuffixToTreeFrom start ft restNew (length commonSuffix + pos)
          (TreeNode [buildEdge (length commonSuffix + subtextStart) restExisting tree])
              
extractCommon :: String -> String -> (String, String, String)
extractCommon [] r = ([], [], r)
extractCommon l [] = ([], l, [])
extractCommon l@(x:xs) r@(e:es) = 
    if x == e 
    then
      let (common, restl, restr) = extractCommon xs es
      in (x:common, restl, restr)
    else ([], l, r)

buildTreeFromScratch :: Int -> [Char] -> Tree
buildTreeFromScratch size text = 
    foldl (addArraySuffix text) rootTree $ generateSuffixes 0 text
    where
      addArraySuffix ft trie (start, text) = addSuffixToTree start ft text trie

slice :: Int -> Int -> [Char] -> [Char]
slice start length text = take length $ drop start text

collectEdges :: [Char] -> Tree -> [String]
collectEdges _ (TreeLeaf _) = []
collectEdges text (TreeNode edges) = foldMap (collectEdgesFromEdge text) edges
  where collectEdgesFromEdge text (TreeEdge char tree) =
          [char] : collectEdges text tree
        collectEdgesFromEdge text (MergedTreeEdge start length tree) =
          slice start length text : collectEdges text tree
  
solve4 :: String -> String
solve4 input =
  let (text:_) = lines input
      size = length text
  in unlines . collectEdges text $ buildTreeFromScratch size text

solve4Trie :: String -> String
solve4Trie input =
  let (text:_) = lines input
  in unlines . collectEdges text
     $ (buildTree . buildTrie) text

main :: IO ()
main = do
  interact solve4
