module Eq3 (Eq3((===))) where


{-
Definition of typeclass Eq3.
The instance definitions should also added.

Task is to define a typeclass Eq3 that introduces a new 3-valued comparison function (===) 
that allows two values of a type that implements Eq3 to be compared with each other, producing a Bool3.

examples:
False3 === True3
Just False3 === Just True3
Just True3 === Nothing

-}

import Bool3

class Eq3 a where
  (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
  _ === Unk3 = Unk3
  Unk3 === _ = Unk3
  False3 === True3 = False3
  True3 === True3 = True3
  False3 === False3 = True3
  _ === _ = False3

instance (Eq3 m) => Eq3 (Maybe m) where
  Just x === Just y = (x === y)
  _ === Nothing = Unk3
  Nothing === _ = Unk3
