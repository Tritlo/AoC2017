{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import AoCUtils

import Text.ParserCombinators.ReadP
import Text.Read hiding (get, choice, (<++))
import Debug.Trace

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)
import Data.Array
import Data.Ix
import Data.Char

import Control.Arrow

import Data.Sequence (Seq(..), Seq, (|>), (<|))
import qualified Data.Sequence as Sq

import Data.Maybe (fromMaybe, isJust)


type Reg = Int

data Ins = Isnd String (Either Int Reg)
         | Ircv String (Either Int Reg)
         | Iset String Reg (Either Int Reg)
         | Iadd String Reg (Either Int Reg)
         | Imul String Reg (Either Int Reg)
         | Imod String Reg (Either Int Reg)
         | Ijgz String Reg (Either Int Reg)

instance Read Ins where
    readPrec = lift parseIns

instance Show Ins where
    show (Isnd s _) = s
    show (Iset s _ _) = s
    show (Iadd s _ _) = s
    show (Imul s _ _) = s
    show (Imod s _ _) = s
    show (Ircv s _) = s
    show (Ijgz s _ _) = s

sEnd :: a -> ReadP a
sEnd = (<* skipSpaces) . pure

intOrReg :: ReadP (Either Int Reg)
intOrReg = (Left <$> parseInt) <++ (Right . ord <$> get)

biOp :: String -> ReadP ((String, Reg), Either Int Reg)
biOp s = swp <$> gather (string s
                         >> skipSpaces
                         >> (,) <$> (ord <$> get >>= sEnd) <*> intOrReg)
 where swp (a,(b,c)) = ((a,b), c)

trim :: String -> String
trim = unwords . words

uniOp :: String -> ReadP (String, Either Int Int)
uniOp s = gather (skipSpaces
                  >> string s
                  >> skipSpaces
                  >> (intOrReg >>= sEnd) )

testInp :: String
testInp = "set a 1 \n\
          \add a 2 \n\
          \mul a a \n\
          \mod a 5 \n\
          \snd a   \n\
          \set a 0 \n\
          \rcv a   \n\
          \jgz a -1\n\
          \set a 1 \n\
          \jgz a -2\n"

parseIns :: ReadP Ins
parseIns = choice [ uncurry Isnd <$> uniOp "snd"
                  , uncurry Ircv <$> uniOp "rcv"
                  , (uncurry . uncurry) Iset <$> biOp  "set"
                  , (uncurry . uncurry) Iadd <$> biOp  "add"
                  , (uncurry . uncurry) Imul <$> biOp  "mul"
                  , (uncurry . uncurry) Imod <$> biOp  "mod"
                  , (uncurry . uncurry) Ijgz <$> biOp  "jgz"
                  ]

data Env = Env { env :: IntMap Int
               , sound :: Maybe Int
               , sent :: Int
               , recv ::  Maybe Int
               , instr :: Int
               , pid :: Int
               , sendsTo :: [Int]
               , getsFrom :: Int
               , waiting :: Bool} deriving (Show)
initEnv :: Env
initEnv = Env M.empty Nothing 0 Nothing 0 0 [0] 0 False

val :: Env -> Either Int Reg -> Int
val _ (Left i) = i
val e (Right c) = fromMaybe 0 $ c `M.lookup` (env e)

eval :: Ins -> Env -> Env
eval (Isnd _ v) e = e {sound = Just nv, sent = (sent e) + 1}
  where nv = val e v
eval (Ircv _ v) e | val e v == 0 = e
eval (Ircv _ v) e = e {recv = (sound e) }
eval (Iset _ reg v) e = e {env = M.insert reg nv (env e)}
  where nv = (val e v)
eval (Iadd _ reg v) e = e {env = M.insert reg nv (env e)}
  where nv = (val e v) + (val e (Right reg))
eval (Imul _ reg v) e = e {env = M.insert reg nv (env e)}
  where nv = (val e v) * (val e (Right reg))
eval (Imod _ reg v) e = e {env = M.insert reg nv (env e)}
  where nv = (val e (Right reg)) `mod` (val e v)
eval (Ijgz _ reg v) e | (val e (Right reg)) > 0 = e {instr = (instr e) + (val e v) -1}
eval (Ijgz _ reg v) e = e


runProgramTillRecv :: Array Int Ins -> Env -> Env
runProgramTillRecv instrs env | (not . null) (recv env) = env
runProgramTillRecv instrs env | (not . inRange b) (instr env)= env
  where b = bounds instrs
runProgramTillRecv instrs env =
    runProgramTillRecv instrs $
    -- traceShowId $
    (incr . eval ins)  env
  where ins =
          -- traceShowId $
          (instrs ! (instr env))



eP :: Array Int Ins -> Int -> State -> State
eP instrs pid s@(es, inb) =
 -- (traceShowId $ traceShow (pid, ins)) $
  evalP pid ins s
  where ins = instrs ! (instr $ (es !!! pid))


type Inboxes = (Seq Int, Seq Int, Seq Int)
type Envs = (Env, Env, Env)
type State = (Envs, Inboxes)

addSend :: Env -> Env
addSend e = e {sent = (sent e) + 1}

execRecv :: Int -> Ins -> State -> State
execRecv pid i@(Ircv _ (Right reg)) (envs, inbs) =
  if (null $ inbs !!! pid) then ( (waitingEnv >!! pid) envs, inbs)
  else ((rcvEnv >!! pid) envs,  ((Sq.deleteAt 0) >!> pid) inbs)
  where curEnv = envs !!! pid
        waitingEnv = curEnv { waiting = True }
        rcvEnv = incr $ curEnv {env = M.insert reg ((inbs !!! pid) `Sq.index` 0) (env curEnv)}

execSend :: Int -> Ins -> State -> State
execSend pid (Isnd _ v) (envs, inbs) = (((incr . addSend) >!> pid) envs,  ninbs)
  where ce = envs !!! pid
        sv = (val ce v)
        md = (|>sv)
        adjusts [] = inbs
        adjusts [0] = ((|>sv) >!> 0) inbs
        adjusts [1,2] = (((|> sv) >!> 1) . ((|>sv) >!> 2)) inbs
        ninbs = adjusts (sendsTo ce)
        -- adjusts = map ((|>sv) >!>) (sendsTo ce)
        -- applAll :: [a -> a] -> a -> a
        -- applAll [] a = a
        -- applAll (f:fs) a = applAll fs (f a)
        -- ninbs = applAll adjusts inbs

incr :: Env -> Env
incr e = e {instr = (instr e) + 1, waiting = False}

evalP :: Int -> Ins ->  State -> State
evalP pid i@(Ircv _ _) s = execRecv pid i s
evalP pid i@(Isnd _ _) s = execSend pid i s
evalP pid ins s@(es, inbs) = (((incr . eval ins) >!> pid) es, inbs)

{-# INLINE (>!>) #-}
(>!>) :: (a -> a) -> Int -> (a, a, a) -> (a, a, a)
(>!>) f 0 (a, b, c) = (f a, b, c)
(>!>) f 1 (a, b, c) = (a, f b, c)
(>!>) f 2 (a, b, c) = (a, b, f c)

{-# INLINE (>!!) #-}
(>!!) ::  a -> Int -> (a, a, a) -> (a, a, a)
(>!!) a 0 (_, b, c) = (a, b, c)
(>!!) b 1 (a, _, c) = (a, b, c)
(>!!) c 2 (a, b, _) = (a, b, c)

{-# INLINE (!!!) #-}
(!!!) :: (a, a, a) -> Int -> a
(!!!) (a, _, _) 0  = a
(!!!) (_, b, _) 1 = b
(!!!) (_, _, c) 2 = c

isDone :: Int -> (Int, Int) -> Envs -> Bool
isDone pid bs = (not . inRange bs) . instr . (!!! pid)

isWaiting :: Int -> Envs -> Bool
isWaiting pid = waiting . (!!! pid)

hasData :: Int -> Inboxes -> Bool
hasData pid = not . null . (!!! pid)


-- Add a "run until send?" to make the memory pressure smaller.
runPrograms :: Array Int Ins -> State -> State
runPrograms instrs !st@(es, _)   | isDone 2 (bounds instrs) es = st
runPrograms instrs !st@(es, ibs) | isWaiting 2 es = case ruHd 2 instrs st of
                                                      Left nst -> runPrograms instrs $ eP instrs 2 $
                                                        --traceShowId
                                                        nst
                                                      Right bst -> bst
runPrograms instrs !st = runPrograms instrs $
  -- traceShowId $
  eP instrs 2 st

toArr :: [a] -> Array Int a
toArr l = listArray (0, length  l - 1) l



pid0 =   initEnv { env = M.singleton (ord 'p') 0, sendsTo = [1,2], getsFrom = 1 }
pid1 =   initEnv { env = M.singleton (ord 'p') 1, sendsTo = [0],   getsFrom = 0 }
pidObs = initEnv { env = M.singleton (ord 'p') 1, sendsTo = [],    getsFrom = 0}


canWork :: Int -> (Int, Int) -> Envs -> Bool
canWork n b es = not (isDone n b es || isWaiting n es)

ruHd :: Int -> Array Int Ins -> State -> Either State State
ruHd n is !st@(_, ibs) | (hasData n ibs) = Left st
ruHd n is !st@(es, _) | (isDone gf (bounds is) es) = Right st
  where gf = getsFrom (es !!! n)
ruHd n is !st@(es, _) | (isWaiting gf es) && (isWaiting gf2 es) = Right st
  where gf = getsFrom (es !!! n)
        gf2 = getsFrom (es !!! gf)
ruHd n is !st@(es, _) | (isWaiting gf es) =
                        -- trace "DaLoop"$
                     case ruHd gf is st of
                       Left nst -> ruHd n is $ eP is gf nst
                       Right bst -> Right bst
  where gf = getsFrom (es !!! n)
        gf2 = getsFrom (es !!! gf)
ruHd n is !st@(es, _) =
  -- traceShow ("lastc", n, gf) $
  ruHd n is $ eP is gf st
  where gf = getsFrom (es !!! n)

initPidEnv :: Int -> Env
initPidEnv pid = initEnv {env = M.singleton (ord 'p') pid, pid = pid}

initProgramEnvs :: Array Int Env
initProgramEnvs = toArr $ map initPidEnv [0,1]

initProgramState :: State
initProgramState = ((pid0,pid1,pidObs), (Sq.empty, Sq.empty, Sq.empty))


test2 = "snd 1\n\
        \snd 2\n\
        \snd p\n\
        \rcv a\n\
        \rcv b\n\
        \rcv c\n\
        \rcv d"

pt2 = (map (read @Ins . trim ) . lines) test2

getAnswer :: State -> Int
getAnswer (es, _)  = sent $ (es !!! 1)
main :: IO ()
main = main2
mainTest = flip runProgramTillRecv initEnv . toArr  .  map (read @Ins . trim) . lines <$> return testInp >>= print
main1 = flip runProgramTillRecv initEnv . toArr  .  map (read @Ins . trim) <$> readInput >>= print
main2 = getAnswer <$> flip runPrograms initProgramState . toArr  .  map (read @Ins . trim) <$> readInput >>= print
mainTest2 = (id &&& getAnswer) <$> flip runPrograms initProgramState . toArr  .  map (read @Ins . trim) . lines <$> return test2 >>= print
