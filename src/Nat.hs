module Nat where

data Nat = Zero | Succ Nat
  deriving (Show, Eq, Ord)

instance Num Nat where
  (+) a Zero = a
  (+) a (Succ b) = Succ (a + b)
  (*) a Zero = Zero
  (*) a (Succ b) = a + (a * b)
  -- No negation op is defined therefore
  negate _ = error "Negate is undefined"
  abs x = x
  signum Zero = Zero
  signum _ = Succ Zero
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n - 1))

-- If i change max to min below type checker will be fine
-- but program is incorrect and only tests can catch the error
beside :: Nat -> Nat -> Bool
beside a b = (Succ $ min a b) == max a b

beside2 :: Nat -> Nat -> Bool
beside2 a b = (Succ $ Succ $ min a b) == max a b

sumNat :: Nat -> Nat -> Nat
sumNat a b = max a b + min a b

pow :: Nat -> Nat -> Nat
pow x Zero = (Succ Zero)
pow x (Succ b) = x * pow x b
