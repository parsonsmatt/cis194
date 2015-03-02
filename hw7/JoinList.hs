module JoinList where
import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


-- Exercise #1:
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append ((tag jl1) <> (tag jl2)) jl1 jl2


tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Exercise #2:

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ x Empty = Nothing
indexJ x (Single s a)
    | x == 0    = Just a
    | otherwise = Nothing
indexJ x (Append m l r)
    | x > gs m  = Nothing
    | x < gs tl = indexJ x l
    | otherwise = indexJ (x - (gs tl)) r
  where tl = tag l
        gs = getSize . size
