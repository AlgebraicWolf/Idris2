module Main

import Data.Vect

-- Cost centres in types must have no effect
eq1 : {0 x : Nat} -> (%costCentre "a cost centre" x) = x
eq1 = Refl

eq2 : {0 n : Nat} -> Vect n String = Vect (%costCentre "a cost centre" n) String
eq2 = Refl

eq3 : (%costCentre "cost centre 1" (1 + 2)) + (3 + 4) = (1 + 2) + (%costCentre "cost centre 2" (3 + 4))
eq3 = Refl
