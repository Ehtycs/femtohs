module TagTypes where


newtype ElementTag = ElementTag Int deriving (Show,Eq, Ord)
newtype NodeTag = NodeTag Int deriving (Show, Eq, Ord)

newtype DimTag = DimTag (Int,Int) deriving (Show, Eq, Ord)
newtype EntityTag = EntityTag Int deriving (Show, Eq, Ord)
