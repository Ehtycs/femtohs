module Element where

type R = Double
type Tag = Int

class Element a where
   elementTag :: a -> Tag

data Node = Node Tag (R,R,R) deriving (Show, Eq)

data Line = Line Tag (Node, Node) deriving (Show, Eq)

instance Element Line where
   elementTag (Line tag _ ) = tag

data Triangle = Triangle Tag (Node, Node, Node) deriving (Show, Eq)

instance Element Triangle where
   elementTag (Triangle tag _) = tag
