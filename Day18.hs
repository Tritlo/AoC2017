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

import Data.Sequence (Seq(..), Seq, (|>))
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
               , sound :: Seq Int
               , sent :: Int
               , recv ::  Maybe Int
               , instr :: Int
               , pid :: Int
               , waiting :: Bool} deriving (Show)

val :: Env -> Either Int Reg -> Int
val _ (Left i) = i
val e (Right c) = fromMaybe 0 $ c `M.lookup` (env e)

eval :: Ins -> Env -> Env
eval (Isnd _ v) e = e {sound = (sound e)|>(val e v), sent = (sent e) + 1}
eval (Ircv _ v) e | val e v == 0 = e
eval (Ircv _ v) e = e {recv = Just lst }
  where (hd:|>lst) = (sound e)
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

incr :: Env -> Env
incr e = e {instr = (instr e) + 1, waiting = False}

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

initEnv :: Env
initEnv = Env M.empty Sq.empty 0 Nothing 0 0 False

eP :: Array Int Ins -> Int -> Array Int Env -> Array Int Env
eP instrs pid envs = evalP pid  ins envs
  where ins = instrs ! (instr $ (envs ! pid))

evalP :: Int -> Ins -> Array Int Env -> Array Int Env
evalP pid (Ircv _ (Right reg)) envs =
  if (null . sound) $ nextEnv then envs // [(pid, waitingEnv)]
   else array b [(pid, rcvdEnv), (nextPid, sentEnv)]
  where b = bounds envs
        nextPid = if pid == 0 then 1 else 0
        curEnv = envs ! pid
        nextEnv = envs ! nextPid
        waitingEnv = curEnv { waiting = True }
        (ini:|>lst) = sound nextEnv
        sentEnv = nextEnv { sound = ini}
        rcvdEnv = incr $ curEnv { env = M.insert reg lst (env curEnv) }
evalP !pid !ins !envs = envs // [(pid, incr $ eval ins (envs ! pid))]


isDone :: Int -> (Int, Int) -> Array Int Env -> Bool
isDone pid bs = (not . inRange bs) . instr . (! pid)
isWaiting :: Int -> Array Int Env -> Bool
isWaiting pid = waiting . ( ! pid)
hasData :: Int -> Array Int Env -> Bool
hasData pid = not . null . sound . ( ! pid)

runPrograms :: Array Int Ins -> Array Int Env -> Array Int Env
runPrograms instrs !envs | not ((isWaiting 0 envs) || isDone 0 (bounds instrs) envs) = runPrograms instrs $ eP instrs 0 envs
runPrograms instrs !envs | not ((isWaiting 1 envs) || isDone 1 (bounds instrs) envs) = runPrograms instrs $ eP instrs 1 envs
runPrograms instrs !envs | (hasData 0 envs) = runPrograms instrs $ eP instrs 1 envs
runPrograms instrs !envs | (hasData 1 envs) = runPrograms instrs $ eP instrs 0 envs

toArr :: [a] -> Array Int a
toArr l = listArray (0, length  l - 1) l


initPidEnv :: Int -> Env
initPidEnv pid = initEnv {env = M.singleton (ord 'p') pid, pid = pid}

initProgramEnvs :: Array Int Env
initProgramEnvs = toArr $ map initPidEnv [0,1]


test2 = "snd 1\n\
        \snd 2\n\
        \snd p\n\
        \rcv a\n\
        \rcv b\n\
        \rcv c\n\
        \rcv d"

getAnswer :: Array Int Env -> Int
getAnswer envs = sent $ (envs ! 1)
main :: IO ()
main = main2
mainTest = flip runProgramTillRecv initEnv . toArr  .  map (read @Ins . trim) . lines <$> return testInp >>= print
main1 = flip runProgramTillRecv initEnv . toArr  .  map (read @Ins . trim) <$> readInput >>= print
main2 = getAnswer <$> flip runPrograms initProgramEnvs . toArr  .  map (read @Ins . trim) <$> readInput >>= print
mainTest2 = flip runPrograms initProgramEnvs . toArr  .  map (read @Ins . trim) . lines <$> return test2 >>= print
