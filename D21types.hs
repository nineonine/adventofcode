{-# LANGUAGE PatternSynonyms #-}

module D21types where

import Control.Monad(guard)

data PlayerType = Human | Computer deriving (Show)

data Player = Player {
       pt   :: PlayerType
    ,  hp   :: Int
    ,  dmg  :: Int
    ,  armr :: Int
    } deriving (Show)

data Item = Item {
       cost   :: Int
    ,  damage :: Int
    ,  armor  :: Int
    } deriving (Show, Eq)

instance Monoid Item where
    mempty = Item 0 0 0
    (Item c1 d1 a1) `mappend` (Item c2 d2 a2) = Item (c1+c2) (d1+d2) (a1+a2)

type Armory = (
      [Item] -- weapons
    , [Item] -- armors
    , [Item]) -- rings


boss :: Player
boss = Player Computer 100 8 2

player :: Player
player = Player Human 100 0 0

allSets :: Armory -> [[Item]]
allSets (ws, arms, rings)= do
    w  <- ws
    a  <- arms
    r1 <- rings
    r2 <- rings
    guard (r1 /= r2 || areEmptyItems r1 r2)
    return [w,a,r1,r2]

areEmptyItems :: Item -> Item -> Bool
areEmptyItems (Item 0 0 0) (Item 0 0 0) = True
areEmptyItems _ _                       = False

equipedIn :: Player -> [Item] -> Player
equipedIn (Player t h dg arm) items = let (Item _ d a) = mconcat items
                                           in Player t h (dg + d) (arm + a)

hitBy :: Player -> Player -> Player
hitBy (Player t1 hp1 dmg1 armr1) (Player t2 hp2 dmg2 armr2 )
      | (hp1+armr1) <= dmg2  = Player t1 0 dmg1 armr1
      | armr1 > dmg2         = Player t1 (hp1-1) dmg1 armr1
      | otherwise            = Player t1 (hp1-(dmg2-armr1)) dmg1 armr1

defeats :: Player -> Player -> Bool
defeats pl1 pl2 = case pl2 `hitBy` pl1 of
                  Player Human 0 _ _     -> False
                  Player Computer 0 _ _  -> True
                  pl2'                   -> defeats pl2' pl1

defeatsIO :: Player -> Player -> IO Bool
defeatsIO pl1 pl2 = do
    print $ pl2 `hitBy` pl1
    case pl2 `hitBy` pl1 of
                Player Human 0 _ _     -> return False
                Player Computer 0 _ _  -> return True
                pl2'                   -> defeatsIO pl2' pl1
