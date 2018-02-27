module D22types where

import Control.Monad
import Control.Monad.State.Lazy

type Duration = Int
type Battle = StateT GameState IO
type Effect = (Spell, Duration)

data GameState = GS { effs :: [Effect], player :: Player, boss :: Player } deriving (Show)

data Player = Player { health :: Int, damage :: Int, armor :: Int, mana :: Int, spells :: [Spell] }
            | Boss   { health :: Int, damage :: Int } deriving (Show)

data Spell = MagicMissle | Drain | Shield | Poison | Recharge deriving (Show, Eq)

magicMissle :: Battle ()
magicMissle = do
    GS efs p b <- get
    let p' = p { mana = mana p - 53 }
        b' = b { health = health b - 4 }
    put $ GS efs p' b'
    return ()

drain :: Battle ()
drain = do
    GS efs p b <- get
    let p' = p { mana = mana p - 73, health = health p + 2 }
        b' = b { health = health b - 2 }
    put $ GS efs p' b'
    return ()

shield :: Battle ()
shield = do
    GS efs p b <- get
    let p' = p { mana = mana p - 113 }
        efs' = (Shield, 6) : efs
    put $ GS efs' p' b
    return ()

poison :: Battle ()
poison = do
    GS efs p b <- get
    let p' = p { mana = mana p - 173 }
        efs' = (Poison, 6) : efs
    put $ GS efs' p' b
    return ()

recharge :: Battle ()
recharge = do
    GS efs p b <- get
    let p' = p { mana = mana p - 229 }
        efs' = (Recharge, 5) : efs
    put $ GS efs' p' b
    return ()

shieldEffect :: Effect -> Battle Effect
shieldEffect (Shield, n) = do
    GS efs p b <- get
    let p' = p { armor = armor p + 7 }
    put $ GS efs p' b
    return (Shield, n-1)

poisonEffect :: Effect -> Battle Effect
poisonEffect (Poison, n) = do
    GS efs p b <- get
    let b' = b { health = health b - 3 }
    put $ GS efs p b'
    return (Poison, n-1)

rechargeEffect :: Effect -> Battle Effect
rechargeEffect (Recharge, n) = do
    GS efs p b <- get
    let p' = p { mana = mana p + 101 }
    put $ GS efs p' b
    return (Recharge, n-1)

runEffect :: Effect -> Battle Effect
runEffect eff = case eff of
    (Shield, _)   -> shieldEffect eff
    (Poison, _)   -> poisonEffect eff
    (Recharge, _) -> rechargeEffect eff

clearWearedOff :: [Effect] -> Battle ()
clearWearedOff efs = do
    GS _ p b <- get
    if (Shield, 0) `elem` efs -- if shield effect is weared off
        then do
             let p' = p { armor = 0 } -- reduce player armor back to 0
                 efs' = filter ( (/=0) . snd ) efs
             put $ GS efs' p' b
             return ()
        else do
             let efs' = filter ( (/=0) . snd ) efs
             put $ GS efs' p b
             return ()

doEffects :: Battle ()
doEffects = do
    efs <- gets effs
    clearWearedOff =<< mapM runEffect efs
    return ()

bossIsDead :: Battle Bool
bossIsDead = do
    Boss hp _ <- gets boss
    if (hp <= 0)
        then return True
        else return False

round :: Player -> Player -> Battle Bool
round p b = do
    if spells p == []
        then return False
        else do
            let nextSpell = head (effects p)
                p `casts` nextSpell
