{-# language FlexibleInstances #-}
{-# language GADTs             #-}
{-# language LambdaCase        #-}
{-# language OverloadedStrings #-}
{-# language Rank2Types        #-}
{-# language TypeOperators     #-}
module GADTs where

import Data.Functor.Classes (liftEq)



















































-- Chapter 1 (0?):
--
-- Evaluating a simple language

data Term0
  = Zero0
  | Succ0   Term0
  | Pred0   Term0
  | IsZero0 Term0
  | If0     Term0 Term0 Term0

data Val
  = IV Int
  | BV Bool

eval0 :: Term0 -> Maybe Val
eval0 = \case
  Zero0 -> Just $ IV 0

  Succ0 tm -> eval0 tm >>= \case
    IV i -> Just $ IV $ succ i
    BV _ -> Nothing
  Pred0 tm -> eval0 tm >>= \case
    IV i -> Just $ IV $ pred i
    BV _ -> Nothing
  IsZero0 tm -> eval0 tm >>= \case
    IV i -> Just $ BV $ i == 0
    BV _ -> Nothing
  If0 cond a b -> eval0 cond >>= \case
    IV _     -> Nothing
    BV condV -> if condV then eval0 a else eval0 b



















































-- Chapter 1.5
--
-- GADT syntax

data Maybe' a where
  Just'    :: a -> Maybe' a
  Nothing' ::      Maybe' a

data Val' ty where
  IV' :: Int  -> Val' Int
  BV' :: Bool -> Val' Bool

-- index vs parameter


















































-- Chapter 2:
--
-- As a GADT

data Term0' where
  Zero0'  :: Term0'
  Succ0'  :: Term0' -> Term0'
  Pred0'  :: Term0' -> Term0'
  IsZero' :: Term0' -> Term0'
  If0'    :: Term0' -> Term0' -> Term0' -> Term0'

data Term t where
  Zero   :: Term Int
  Succ   :: Term Int  -> Term Int
  Pred   :: Term Int  -> Term Int
  IsZero :: Term Int  -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

eval :: Term t -> t
eval = \case
  Zero        -> 0
  Succ tm     -> succ $ eval tm
  Pred tm     -> pred $ eval tm
  IsZero tm   -> eval tm == 0
  If cond a b -> if eval cond then eval a else eval b



















































-- Chapter 2.5
--
-- GADTs desugared

data ExTerm t where
  ExZero   :: t ~ Int  => ExTerm t
  ExSucc   :: t ~ Int  => ExTerm Int  -> ExTerm t
  ExPred   :: t ~ Int  => ExTerm Int  -> ExTerm t
  ExIsZero :: t ~ Bool => ExTerm Int  -> ExTerm t
  ExIf     ::             ExTerm Bool -> ExTerm t -> ExTerm t -> ExTerm t

















































-- Chapter 3:
--
-- Generic functions

data Type t where
  RInt  :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)

rString :: Type String
rString = RList RChar

type Bit = Bool

serialize :: Type t -> t -> [Bit]
serialize RInt          i        = serializeInt i
serialize RChar         c        = serializeChar c
serialize (RList _)     []       = False : []
serialize (RList ra)    (a : as) = True : serialize ra a ++ serialize (RList ra) as
serialize (RPair ra rb) (a, b)   = serialize ra a ++ serialize rb b

serializeInt :: Int -> [Bit]
serializeInt = undefined

serializeChar :: Char -> [Bit]
serializeChar = undefined

eq :: Type t -> t -> t -> Bool
eq RInt            a        b        = a == b
eq RChar           a        b        = a == b
eq (RList ty)      a        b        = liftEq (eq ty) a b
eq (RPair tya tyb) (a1, b1) (a2, b2) = eq tya a1 a2 && eq tyb b1 b2



















































-- Chapter 4:
--
-- Existentials

-- Data.Type.Equality ((:~:)(Refl))
data a :~: b where
  Refl :: a :~: a

data Existential where
  Some :: Type t -> t -> Existential

tEqual :: Type a -> Type b -> Maybe (a :~: b)
tEqual RInt      RInt      = Just Refl
tEqual RChar     RChar     = Just Refl
tEqual (RList a) (RList b) = do
  Refl <- tEqual a b
  pure Refl
tEqual (RPair a1 b1) (RPair a2 b2) = do
  Refl <- tEqual a1 a2 
  Refl <- tEqual b1 b2
  pure Refl
tEqual _ _ = Nothing

instance Eq Existential where
  Some ty1 t1 == Some ty2 t2 = case tEqual ty1 ty2 of
    Nothing   -> False
    Just Refl -> eq ty1 t1 t2

from :: a :~: b -> (a -> b)
from Refl = id

to :: a :~: b -> (b -> a)
to Refl = id



















































-- Chapter 5
--
-- Type-safe formatting

data Dir x y where
  Lit    :: String             -> Dir x x
  Int    ::                       Dir x (Int -> x)
  String ::                       Dir x (String -> x)
  (:^:)  :: Dir b c -> Dir a b -> Dir a c

format' :: Dir x y -> (String -> x) -> (String -> y)
format' (Lit s)     = \cont out   -> cont (out ++ s)
format' Int         = \cont out i -> cont (out ++ show i)
format' String      = \cont out s -> cont (out ++ s)
format' (d1 :^: d2) = format' d1 . format' d2

format :: Dir String y -> y
format d = format' d id ""

formatExamples :: [String]
formatExamples  =
  [ format $ Lit "Richard"
  , format Int 60
  , format (String :^: Lit " is " :^: Int) "Richard" 60
  ]



















































-- Chapter 6
--
-- via https://gist.github.com/rampion/2659812

data Zero 
data Succ n
type One = Succ Zero

data Red
data Black

data Node c n a where
  -- all leafs are black
  Leaf  :: Node Black One a 
  -- internal black nodes can have children of either color
  B     :: Node cL n a    -> a -> Node cR n a    -> Node Black (Succ n) a 
  -- internal red nodes can only have black children
  R     :: Node Black n a -> a -> Node Black n a -> Node Red n a

-- Many examples from Ralf Hinze -- Fun with phantom types
