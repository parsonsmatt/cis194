module Employee where
import qualified Data.Foldable as F
import Data.Tree
import Data.Monoid

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

-- Exercise #1:

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps funScore) = GL (emp:emps) (funScore + empFun emp) 


instance Monoid GuestList where
    mempty  = GL [] 0
    mappend (GL emps1 f1) (GL emps2 f2) = GL (emps1 ++ emps2) (f1 + f2)  


moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL emps1 f1) (GL emps2 f2)
    | f1 > f2   = GL emps1 f1
    | otherwise = GL emps2 f2

-- Exercise 2:

foldTree :: (Monoid b) => (a -> b -> b) -> b -> Tree a -> b
foldTree f start tree = mconcat (map (foldTree f mempty) branches) <> f root start
                    where
                        root     = rootLabel tree
                        branches = subForest tree

-- For whatever reason, the above fold wasn't playing nicely...
foldTree' :: (a -> [b] -> b) -> Tree a -> b
foldTree' f tree = f root $ map (foldTree' f) branches
               where
                   root     = rootLabel tree
                   branches = subForest tree

-- Exercise 3:

nextLevel :: Employee -> 
             -- The boss of the current subtree
             [(GuestList, GuestList)] -> 
             -- List of results for each subtree under the boss. First GuestList
             -- is a best guest list with the boss of that tree. Second is best
             -- GuestList without the boss of that tree.
             (GuestList, GuestList)
             -- (Sublist w/ boss, sublist w/o boss) 
nextLevel emp []        = (glCons emp mempty, mempty)
nextLevel boss sublists = (bestWithBoss, bestWithoutBoss)
                      where 
                          bestWithBoss    = glCons boss noSubordinates
                          bestWithoutBoss = mconcat $ map fst sublists
                          noSubordinates  = mconcat $ map snd sublists


maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . foldTree' nextLevel


