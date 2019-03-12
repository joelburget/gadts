{-# language FlexibleInstances #-}
{-# language GADTs             #-}
{-# language LambdaCase        #-}
{-# language OverloadedStrings #-}
{-# language Rank2Types        #-}
{-# language TypeOperators     #-}
module GADTs where

-- import Data.Functor.Classes (liftEq)



















































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

  _ -> error "TODO"



















































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
  _ -> error "TODO"



















































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
eq = error "TODO"



















































-- Chapter 4:
--
-- Existentials

-- Data.Type.Equality ((:~:)(Refl))
data a :~: b where
  Refl :: a :~: a

tEqual :: Type a -> Type b -> Maybe (a :~: b)
tEqual _ _ = error "TODO"

data Existential where
  Some :: Type t -> t -> Existential

instance Eq Existential where
  (==) = error "TODO"

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
