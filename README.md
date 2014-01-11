#mini-reg

mini regex implementation.
This is my learning project.

##Feature

Reguler matching on Ord instances List.

```haskell
import qualified Reg as R
import qualified EpsilonNFA as N
import qualified DFA as D
import qualified NFABuilder as B

-- Normal Reguler Expression.
reg = (R.and (R.get 'a') (R.get 'b'))
dfa = N.genDFA $ B.buildNFA reg
D.accept dfa "ab"
-- => True
D.accept dfa "abb"
-- => False

-- Matching on Ord instances List.
oreg = (R.and (R.is even) (R.star (R.is odd)))
odfa = N.getnDFA $ B.buildNFA oreg
D.accept odfa [2]
-- => True
D.accept odfa [2,3]
-- => True
D.accept odfa [2,3,5,7]
-- => True
```
