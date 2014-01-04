{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EpsilonNFA where

import Data.Set
import qualified Data.List as L
import Control.Monad
import Control.Lens
import qualified DFA as D (State(State), Input(Input), Rule(Rule), DFA, mkDFA)
import qualified Debug.Trace as De

data Input a = Input a | Epsilon deriving (Show, Eq)
             
data Rule a b = Rule (D.State a) (Input b) (D.State a) deriving (Show, Eq)

matchRule :: (Eq a, Eq b) => Input b -> D.State a -> [Rule a b] -> [Rule a b]
matchRule i cs rs = 
    L.filter (\(Rule c ii _) -> (cs == c) && (i == ii)) rs

--Epsilonルール１ステップで移動可能な状態の集合を得る
eclose' :: (Eq a, Eq b) => D.State a -> [Rule a b] -> [D.State a]
eclose' s rs = enext
  where
    erule = L.filter (\(Rule x i _) -> (x == s) && (i == Epsilon)) rs
    enext = L.map (\(Rule _ _ ns) -> ns) erule

--Epsilonルールで移動可能な状態の集合を得る
eclose'' :: (Eq a, Eq b) => [D.State a] -> [Rule a b] -> [D.State a] -> [D.State a]
eclose'' s rs acc
  | epNexts == [] = acc
  | otherwise = eclose'' s rs (acc ++ epNexts)
  where
    epNexts = L.filter (\es -> not (elem es acc)) $ 
                concat $ L.map (\a -> eclose' a rs) acc

eclose :: (Eq a, Eq b) => [D.State a] -> [Rule a b] -> [D.State a]
eclose s rs = eclose'' s rs s

data EpsilonNFA a b = EpsilonNFA {
                _fstState :: D.State a
                ,_currState :: [D.State a]
                ,_rules :: [Rule a b]
                ,_goalState :: [D.State a]
                } deriving (Show, Eq)
$(makeLenses ''EpsilonNFA)

mkEpsilonNFA :: (Eq a, Eq b) => D.State a -> [Rule a b] -> [D.State a] -> EpsilonNFA a b
mkEpsilonNFA s rs gs = EpsilonNFA {
                _fstState = s
                ,_currState = eclose [s] rs
                ,_rules = rs
                ,_goalState = gs
                }

updateEpsilonNFA :: (Eq a, Eq b) => EpsilonNFA a b -> Input b -> Maybe (EpsilonNFA a b)
updateEpsilonNFA enfa i = updateEpsilonNFA' enfa nxtStates
  where
    rs = concat $ L.map (\s -> matchRule i s (enfa^.rules))
                        (enfa^.currState)
    nxtStates = eclose (L.map (\(Rule _ _ ns) -> ns) rs) (enfa^.rules)
    updateEpsilonNFA' :: (Eq a, Eq b) => EpsilonNFA a b -> [D.State a] -> Maybe (EpsilonNFA a b)
    updateEpsilonNFA' _ [] = Nothing
    updateEpsilonNFA' nfa ns = Just (nfa&currState.~ns)

runEpsilonNFA :: (Eq a, Eq b) => EpsilonNFA a b -> [Input b] -> Maybe (EpsilonNFA a b)
runEpsilonNFA enfa is = foldM updateEpsilonNFA enfa is

accept :: (Eq a, Eq b) => EpsilonNFA a b -> [Input b] -> Bool
accept enfa is = accept' res
  where
    res = runEpsilonNFA enfa is
    accept' Nothing = False
    accept' (Just f) = L.any (\s -> elem s (f^.goalState)) (f^.currState)

{-
- Convert Epsilon-NFA -> DFA
-}

enfaInput2dfaInput :: Input a -> D.Input a
enfaInput2dfaInput (Input a) = D.Input a
enfaInput2dfaInput (Epsilon) = error "Can't convert epsilon to dfa-input"

type ENFARule a b = Rule a b
type DFARule a b = D.Rule (Set a) b
genDFARule' :: forall a b. (Eq a, Eq b, Ord a) => (D.State (Set a)) -> [ENFARule a b] -> [DFARule a b]
genDFARule' (D.State s) rs = L.map nxt matchedRules
  where
    matchedRules :: [ENFARule a b] 
    matchedRules = L.filter (\(Rule (D.State x) i _) -> (member x s) && (i /= Epsilon)) rs
    nxt ::  ENFARule a b -> DFARule a b
    nxt (Rule (D.State f) i t) = D.Rule (D.State s) (enfaInput2dfaInput i) (D.State (fromList (L.map (\(D.State x) -> x) (eclose [t] rs))))

genDFARule'' :: (Eq a, Eq b, Ord a, Show a, Show b) => [ENFARule a b] -> [DFARule a b] -> [DFARule a b] -> [DFARule a b]
genDFARule'' rs acc tmpAcc
  | newRules == [] = acc ++ newRules
  | otherwise = genDFARule'' rs (newRules ++ acc) newRules
  where
    generatedRules = concat $ L.map (\(D.Rule _ _ x) -> genDFARule' x rs) tmpAcc
    newRules = L.filter (\x -> not ((elem x acc) || (elem x tmpAcc))) generatedRules

genDFA :: forall a b. (Eq a, Eq b,Ord a, Show a, Show b) => EpsilonNFA a b -> D.DFA (Set a) b
genDFA enfa = D.mkDFA fst dfaRules dfaGoal
  where
    fstClose = eclose [(enfa^.fstState)] (enfa^.rules)
    fst :: D.State (Set a)
    fst = D.State $ fromList $ L.map (\(D.State x) -> x) fstClose
    enfaRules = enfa^.rules
    fstDFARules = genDFARule' fst enfaRules
    dfaRules = (genDFARule'' enfaRules fstDFARules fstDFARules)
    dfaGoal = L.filter (\(D.State t) -> any (\(D.State x) -> member x t) (enfa^.goalState)) $ L.map (\(D.Rule _ _ g) -> g) dfaRules
