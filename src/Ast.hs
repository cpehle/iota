module Ast where

import Data.Set

data Type = TArrow Type Type
          | TDouble
          deriving Eq

data Constant = Double Double

data IsRec = Rec | NonRec

type Definition a = (a, TermForm a ())

data TermForm a b = C Constant
            | V a b
            | App (TermForm a b) (TermForm a b)
            | Fun [a] (TermForm a b)
            | Let IsRec a [Definition a] (TermForm a b)

type Term = TermForm String ()
type TTerm = TermForm (String, Type) ()

type AnnTerm a b = (b, TermForm a b)
type AnnDefn a b = (a, AnnTerm a b)


type ScDefn = (String, [String], Term)

freeVars :: Term -> AnnTerm String (Set String)
freeVars (C c) = ([], AConst k)
freeVars (V v) = ([v], AVar v)
freeVars (Ap e1 e2) = ((freeVarsOf e1') ++ (freeVarsOf e2'), AAp e1' e2')
                      where e1' = freeVars e1
                            e2' = freeVars e2

abstract :: AnnTerm String (Set String) -> Term
abstract = undefined


collectScs :: Term -> [ScDefn]
collectScs = undefined


lambdaLift :: Term -> [ScDefn]
lambdaLift = collectScs . abstract . freeVars


typeCheck = undefined
closureConvert = undefined
