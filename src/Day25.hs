
module Day25 (day25) where

import MyLib hiding (Tree)
import Data.Set as Set
import qualified Data.Set as Set
-- import Data.STRef
-- import Control.Monad.ST 
import Control.Monad (forM_)
import Control.Effect.State
import Control.Carrier.State.Strict

data Turing = Turing {position :: Int, tape :: Set Int, machineState :: MachineState} deriving (Show, Eq)
data MachineState = A | B | C | D | E | F deriving (Show, Eq)

step :: Turing -> Turing
step (Turing position tape machineState) = case machineState of
  A -> if position `Set.member` tape
       then Turing (position - 1) (Set.delete position tape) F  -- 1
       else Turing (position + 1) (Set.insert position tape) B  -- 0
  B -> if position `Set.member` tape
       then Turing (position + 1) (Set.delete position tape) D  -- 1
       else Turing (position + 1) tape C                        -- 0
  C -> if position `Set.member` tape
       then Turing (position + 1) tape E                        -- 1
       else Turing (position - 1) (Set.insert position tape) D  -- 0
  D -> if position `Set.member` tape
       then Turing (position - 1) (Set.delete position tape) D  -- 1
       else Turing (position - 1) tape E                        -- 0
  E -> if position `Set.member` tape
       then Turing (position + 1) tape C                        -- 1
       else Turing (position + 1) tape A                        -- 0
  F -> if position `Set.member` tape
       then Turing (position + 1) tape A                        -- 1
       else Turing (position - 1) (Set.insert position tape) A  -- 0

checkStep :: Int
checkStep = 12994925

initTuring :: Turing
initTuring = Turing 0 Set.empty A

-- finalTuring :: Turing
-- finalTuring = runST $ do
--   turing <- newSTRef initTuring
--   forM_ [1..checkStep] $ \_ -> do
--     modifySTRef' turing step
--   readSTRef turing

finalTuring' :: Turing
finalTuring' = run . execState initTuring $ do
  forM_ [1..checkStep] $ \_ -> modify step

day25 :: IO ()
day25 = do
  putStrLn ("day25a: " ++ show (length finalTuring'.tape))
  putStrLn ("day25b: " ++ "Merry Christmas!")