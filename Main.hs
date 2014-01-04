module Main where

import qualified DFA as D (State(State), accept, Input(Input))
import qualified EpsilonNFA as N (Input(Input, Epsilon), Rule(Rule), eclose, mkEpsilonNFA, runEpsilonNFA, accept, genDFA)
import qualified NFABuilder as B
import qualified Reg as R

reg = ((R.get 'R') R.|| (R.get 'S')) R.&& (R.lone (R.get 'X')) R.&& (R.get 'Y') R.&& (R.some (R.get 'Z'))
enfa = B.buildNFA reg
dfa = N.genDFA enfa

main :: IO ()
main = do {
          putStrLn $ show $ reg;
          putStrLn $ show $ D.accept dfa $ map (\c -> (D.Input c)) "SXYZ";
          putStrLn $ show $ D.accept dfa $ map (\c -> (D.Input c)) "SXXYZ";
        }
