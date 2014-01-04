{-# LANGUAGE TemplateHaskell #-}
module NFABuilder where

import qualified DFA as D (State(State))
import qualified EpsilonNFA as N (Input(Input, Epsilon), Rule(Rule), EpsilonNFA(EpsilonNFA), mkEpsilonNFA)
import qualified Reg as R (Reg, RegTerm(Take), RegExp(Or, And, Star, Lone, Some))

buildNFA' :: R.Reg a -> ([N.Rule Int a], Int) -> ([N.Rule Int a], Int)
buildNFA' (Left (R.Take a)) (acc, s) = (newRule : acc, (s + 1))
  where
      newRule = N.Rule (D.State s) (N.Input a) (D.State (s + 1))

buildNFA' (Right (R.Or a b)) st@(acc, s) = (newAcc, bl + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    (acc2, bl) = buildNFA' b (acc1, al + 1)
    epRule1 = N.Rule (D.State s) (N.Epsilon) (D.State (s + 1))
    epRule2 = N.Rule (D.State s) (N.Epsilon) (D.State (al + 1))
    epRule3 = N.Rule (D.State al) (N.Epsilon) (D.State (bl + 1))
    epRule4 = N.Rule (D.State bl) (N.Epsilon) (D.State (bl + 1))
    newAcc = [epRule1, epRule2, epRule3, epRule4] ++ acc2

buildNFA' (Right (R.And a b)) st@(acc, s) = (newAcc, bl + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    (acc2, bl) = buildNFA' b (acc1, al + 1)
    epRule1 = N.Rule (D.State s) (N.Epsilon) (D.State (s + 1))
    epRule2 = N.Rule (D.State al) (N.Epsilon) (D.State (al + 1))
    epRule3 = N.Rule (D.State bl) (N.Epsilon) (D.State (bl + 1))
    newAcc = [epRule1, epRule2, epRule3] ++ acc2

buildNFA' (Right (R.Star a)) st@(acc, s) = (newAcc, al + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    epRule1 = N.Rule (D.State s) N.Epsilon (D.State (s + 1))
    epRule2 = N.Rule (D.State al) N.Epsilon (D.State (s + 1))
    epRule3 = N.Rule (D.State al) N.Epsilon (D.State (al + 1))
    epRule4 = N.Rule (D.State s) N.Epsilon (D.State (al + 1))
    newAcc = [epRule1, epRule2, epRule3, epRule4] ++ acc1

buildNFA' (Right (R.Lone a)) st@(acc, s) = (newAcc, al + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    epRule1 = N.Rule (D.State s) N.Epsilon (D.State (s + 1))
    epRule2 = N.Rule (D.State al) N.Epsilon (D.State (al + 1))
    epRule3 = N.Rule (D.State s) N.Epsilon (D.State (al + 1))
    newAcc = [epRule1, epRule2, epRule3] ++ acc1

buildNFA' (Right (R.Some a)) st@(acc, s) = (newAcc, bl + 1)
  where
    (acc1, al) = buildNFA' a (acc, s + 1)
    (acc2, bl) = buildNFA' (Right (R.Star a)) (acc1, al + 1)
    epRule1 = N.Rule (D.State s) N.Epsilon (D.State (s + 1))
    epRule2 = N.Rule (D.State al) N.Epsilon (D.State (al + 1))
    epRule3 = N.Rule (D.State bl) N.Epsilon (D.State (bl + 1))
    newAcc = [epRule1, epRule2, epRule3] ++ acc2

buildNFA :: Eq a => R.Reg a -> N.EpsilonNFA Int a
buildNFA reg = N.mkEpsilonNFA (D.State 0) rules [(D.State last)]
  where
    (rules, last) = buildNFA' reg ([], 0)
