{-# LANGUAGE TemplateHaskell #-}
module NFA where

import Data.List
import Control.Monad
import Control.Lens
import qualified DFA as D (Input(Input), State(State), Rule(Rule))

matchRule :: Eq a => D.Input a -> D.State -> [D.Rule a] -> [D.Rule a]
matchRule i cs rs = 
    filter (\(D.Rule c ii _) -> (cs == c) && (i == ii)) rs

data NFA a = NFA {
         _fstState :: D.State
         ,_currState :: [D.State]
         ,_rules :: [D.Rule a]
         ,_goalState :: [D.State]
} deriving (Show, Eq)
$(makeLenses ''NFA)


mkNFA :: D.State -> [D.Rule a] -> [D.State] -> NFA a
mkNFA s rs gs = NFA {
          _fstState = s
          ,_currState = [s]
          ,_rules = rs
          ,_goalState = gs}

updateNFA :: Eq a => NFA a -> D.Input a -> Maybe (NFA a)
updateNFA nfa i = updateNFA' nfa nxtStates
  where
    rs = concat $ map (\s -> matchRule i s (nfa^.rules)) 
                        (nfa^.currState)
    nxtStates = map (\(D.Rule _ _ ns) -> ns) rs
    updateNFA' :: Eq a => NFA a -> [D.State] -> Maybe (NFA a)
    updateNFA' _ [] = Nothing
    updateNFA' nfa ns = Just (nfa&currState.~ns)

runNFA :: Eq a => NFA a -> [D.Input a] -> Maybe (NFA a)
runNFA nf is = foldM updateNFA nf is

accept :: Eq a => NFA a -> [D.Input a] -> Bool
accept nfa is = accept' res
  where
    res = runNFA nfa is
    accept' Nothing = False
    accept' (Just dfa) = any (\s -> elem s (dfa^.goalState))
                                (dfa^.currState)
