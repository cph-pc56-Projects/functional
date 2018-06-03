import Data.IORef

data Tree d = EmptyNode | Node d (Tree d) (Tree d)
 deriving Show

empty :: Ord d => Tree d -> Bool
empty EmptyNode = True
empty  _  = False

contains :: Ord d => Tree d -> d -> Bool
contains EmptyNode _ = False
contains (Node nodeValue left right) value 
 | value == nodeValue = True
 | value  < nodeValue = contains left value 
 | value  > nodeValue = contains right value

insert :: Ord d => Tree d -> d -> Tree d
insert EmptyNode nodeValue = Node nodeValue EmptyNode EmptyNode
insert (Node nodeValue left right) value
 | nodeValue == value = Node nodeValue left right
 | nodeValue < value  = Node nodeValue (insert left value) right
 | nodeValue > value  = Node nodeValue left (insert right value)

-- Create tree from list of elemtents
ctree :: Ord d => [d] -> Tree d 
ctree [] = EmptyNode
ctree (h:t) = ctree2 (Node h EmptyNode EmptyNode) t
 where
  ctree2 tr [] = tr
  ctree2 tr (h:t) = ctree2 (insert tr h) t

{- PRINT -}
inorder :: Ord d => Tree d -> [d]
inorder EmptyNode = []
inorder (Node nodeValue left right) = inorder left ++ [nodeValue] ++ inorder right

preorder :: Ord d => Tree d -> [d]
preorder EmptyNode = []
preorder (Node nodeValue left right) = [nodeValue] ++ preorder left ++ preorder right

postorder :: Ord d => Tree d -> [d]
postorder EmptyNode = []
postorder (Node nodeValue left right) = postorder left ++ postorder right ++ [nodeValue]

main :: IO ()
main = do
 treeRef <- newIORef EmptyNode
 writeIORef treeRef (Node 7 EmptyNode EmptyNode)
 tree <- readIORef treeRef
 putStrLn (show tree)
 writeIORef treeRef (insert tree 8)
 --putStrLn (inorder tree)
