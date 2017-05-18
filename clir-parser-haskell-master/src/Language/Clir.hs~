{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Clir (
            ClirType (..)
          , TypedVar (..)
          , VarName
          , ConstValue (..)
          , FunName
          , FunctionDefinition (..)
          , DataConstructor
          , CaseAltPattern (..)
          , CaseAltExpr (..)
          , AtomicExpression (..)
          , BindingExpression (..)
          , GeneralExpression (..)
          , TopLevelDef (..)
          ) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.List as L
import qualified Language.Sexp as Sexp
import Language.SexpGrammar
import Language.SexpGrammar.Generic

import GHC.Generics (Generic)
-----------------------------------------------------------------------------------------
-- The Sexp library provides an abstract syntax for S-expressions:
--  (Position is the position of the expression in the input file)
--
--  data Sexp = Atom   Position ...    -- six variants of atoms
--            | List   Position [Sexp] -- S-expression list enclosed into '(' ')'
--            | Vector Position [Sexp] -- S-expression list enclosed into '[' ']'



-----------------------------------------------------------------------------------------
-- The first parsing phase is from an S-expression to the CLIR abstract syntax
-- Type definition for the CLIR abstract syntax
-----------------------------------------------------------------------------------------

data ClirType = UnitType
              | SimpleType Text
              | TypeVar Text
              | CompoundType [ClirType]
              deriving (Show, Eq, Ord, Generic)

-----------------------------------------------------------------------------------------
-- The second parsing phase is from the CLIR abstract syntax to the IR AST
-- Type definitions for the IR abstract syntax
-----------------------------------------------------------------------------------------


-- Typed variable definitions

data TypedVar = TypedVar String ClirType
              deriving (Show, Eq, Ord, Generic)

type VarName = String

data ConstValue = ConstString String
                | ConstInt Int
                | ConstNumber Scientific
                | ConstBool Bool
                deriving (Show, Eq, Ord, Generic)

type FunName = String

data FunctionDefinition = FunDef FunName [TypedVar] [TypedVar] [Contract] GeneralExpression
                          deriving (Show, Eq, Ord, Generic)

type DataConstructor = String


data CaseAltPattern = CConstant ConstValue ClirType
                    | CConstructor DataConstructor [AtomicExpression]
                    | CDefault
                    deriving (Show, Eq, Ord, Generic)

data CaseAltExpr = CaseAlt CaseAltPattern GeneralExpression
                   deriving (Eq, Ord, Generic)

instance Show CaseAltExpr where
  showsPrec _ (CaseAlt pat e) = 
      shows pat . showString " --> " . shows e

-- Atomic Expressions
data AtomicExpression = Var VarName
                      | Const ConstValue ClirType
                      deriving (Show, Eq, Ord, Generic)

-- Binding Expressions
data BindingExpression = AtomE AtomicExpression
                       | FunA FunName [AtomicExpression]
                       | ConstrA DataConstructor [AtomicExpression]
                       | Tuple [AtomicExpression]
                       deriving (Show, Eq, Ord, Generic)

-- General Expressions
data GeneralExpression = Binding BindingExpression
                       | Let [TypedVar] BindingExpression GeneralExpression
                       | LetFun [FunctionDefinition] GeneralExpression
                       | Case AtomicExpression [CaseAltExpr]
                       deriving (Eq, Ord, Generic)

instance Show GeneralExpression where
  showsPrec _ (LetFun fs e) = 
        showString "\nletfun\n   " . 
        (showString $ L.intercalate "\n   " (map show fs)) .
        showString "\n\n" . shows e
  showsPrec _ (Binding be) = shows be
  showsPrec _ (Let vs be e) = 
        showString "\nlet " . shows vs . showString " = " . shows be . showString " in" .
        showString "\n   " . shows e 
  showsPrec _ (Case x alts) =
        showString "\ncase " . shows x . showString " of\n   " . 
        (showString $ L.intercalate "\n   " (map show alts))

data TopLevelDef = TopFunDef FunName [TypedVar] [TypedVar] [Contract] GeneralExpression
                 | OtherInfo String String String String 
                 deriving (Eq, Ord, Generic)

instance Show TopLevelDef where
  showsPrec _ (TopFunDef fn args ress cs e) =
     showString ("\n\ndefine " ++ fn ++ show args ++ show ress ++ "\n") .
     (showString $ L.intercalate "\n   " (map show cs)) .
     showString ("\n") . shows e
  showsPrec _ (OtherInfo s1 s2 s3 s4) = showString $ L.intercalate "\n"
                                        ["\nverification unit: "++s1,s2,s3,s4]


-- The AST for contracts

data Contract = SpecContract [Spec]
              | OtherContract
              deriving (Show, Eq, Ord, Generic)

data Spec     = PreCD Assertion
              | PostCD Assertion
              | OtherAssertion Assertion
              deriving (Eq, Ord, Generic)

instance Show Spec where
  showsPrec _ (PreCD ass) = 
     showString ("\n\nRequires ") . shows ass 
  showsPrec _ (PostCD ass) = 
     showString ("\n\nEnsures ") . shows ass 
  showsPrec _ (OtherAssertion ass) = 
     showString ("\n\nAssert ") . shows ass 







type PredName = String

data Assertion = ATrue
               | AFalse
               | Not Assertion
               | And [Assertion]
               | Or [Assertion]
               | Imp [Assertion]
               | Equiv [Assertion]
               | Aplic PredName [BindingExpression]
               | Forall [TypedVar] Assertion
               | Exists [TypedVar] Assertion
               deriving (Show, Eq, Ord, Generic)

-----------------------------------------------------------------------------------------
-- Defining instances of these types for the class SexpIso (Sexp Isomorphisms)
-- This is the first phase parser: from S-exp to CLIR AST
-----------------------------------------------------------------------------------------


typeSig :: Grammar g (Sexp :- t) (ClirType :- t)
typeSig = iso fromSexp toSexp
  where fromSexp (Atom _ (AtomSymbol a)) = SimpleType a
        fromSexp (Atom _ _) = error "Unexpected atom type different from symbol"
        fromSexp (List _ []) = UnitType
        -- fromSexp (List _ [Quoted _ (Atom _ (AtomSymbol a))]) = TypeVar a
        fromSexp (List _ l) = CompoundType (map fromSexp l)
        fromSexp (Vector _ _) = error "Unexpected vector in type"
        fromSexp (Quoted _ (Atom _ (AtomSymbol a))) = TypeVar a
        fromSexp (Quoted _ a) = error $ "Unexpected quoted value" ++ show a
        toSexp (SimpleType a) = (Atom Sexp.dummyPos (AtomSymbol a))
        toSexp (UnitType) = (List Sexp.dummyPos [])
        -- toSexp (TypeVar a) = (List Sexp.dummyPos [(Quoted Sexp.dummyPos (Atom Sexp.dummyPos (AtomSymbol a)))])
        toSexp (TypeVar a) = Quoted Sexp.dummyPos (Atom Sexp.dummyPos (AtomSymbol a))
        toSexp (CompoundType l) = (List Sexp.dummyPos (map toSexp l))

instance SexpIso ClirType where
  sexpIso = typeSig



-----------------------------------------------------------------------------------------
-- Here it starts the second phase parser: from CLIR AST to IR AST
-----------------------------------------------------------------------------------------


-- This is to combine a bool grammar, and a true/false constant into a bool result
clirbool :: Grammar SexpGrammar (Sexp :- b) (Bool :- b)
clirbool = coproduct [ bool, sym "true" >>> push True, sym "false" >>> push False ]


-- | Constant nodes are used in different nodes of the grammar (case
-- alternatives and constants).
constantNode :: Grammar SexpGrammar (Sexp :- t) (ConstValue :- t)
constantNode = match
               $ With (\str -> str . string')
               $ With (\int_ -> int_ . int)
               $ With (\real_ -> real_ . real)
               $ With (\bool_ -> bool_ . clirbool)
               $ End


clir_constant :: Grammar SexpGrammar (Sexp :- t) (ClirType :- (ConstValue :- t))
clir_constant = (list (
                   el (sym "the")  >>>
                   el sexpIso      >>>
                   el constantNode >>> swap))


-- | Constructor applications are used in the grammar both for
-- destructuring in case alternatives and literal constructor
-- applications
clir_constructorApp :: Grammar SexpGrammar (Sexp :- b) ([AtomicExpression] :- (String :- b))
clir_constructorApp = (list (el (sym "@@") >>>
                             el symbol' >>>
                             rest sexpIso))


instance SexpIso AtomicExpression where
  sexpIso = match
    $ With (\var -> var . symbol')
    $ With (\const -> const . clir_constant)
    $ End


-- Binding Expressions
-- data BindingExpression = AtomE AtomicExpression
--                        | FunA FunName [AtomicExpression]
--                        | ConstrA DataConstructor [AtomicExpression]
--                        | Tuple [AtomicExpression]
--                        deriving (Show, Eq, Ord, Generic)

instance SexpIso BindingExpression where
  sexpIso = match
    $ With (\atome -> atome . sexpIso)
    $ With (\funa -> funa . (list (el (sym "@") >>>
                                   el symbol' >>>
                                   rest sexpIso)))
    $ With (\constra -> constra . clir_constructorApp)
    $ With (\tuple -> tuple . (list (el (sym "tuple") >>>
                                     rest sexpIso)))
    $ End


typedVar :: Grammar SexpGrammar (Sexp :- t) (TypedVar :- t)
typedVar = match
  $ With (\typedvar -> typedvar . list (el symbol' >>> el typeSig))
  $ End

instance SexpIso TypedVar where
  sexpIso = typedVar


typedVarList :: Grammar SexpGrammar (Sexp :- t) ([TypedVar] :- t)
typedVarList = sexpIso


funDecl :: Grammar SexpGrammar (Sexp :- t) (FunctionDefinition :- t)
funDecl = match
  $ With (\fdef -> fdef . coproduct [ list (el symbol'      >>>
                                            el typedVarList >>>
                                            el typedVarList >>>
                                            el contracts    >>>
                                            el sexpIso)
                                    , list (el symbol'      >>>
                                            el typedVarList >>>
                                            el typedVarList >>>
                                            push []         >>>
                                            el sexpIso)
                                    ])
  $ End


instance SexpIso FunctionDefinition where
  sexpIso = funDecl


funDeclList :: Grammar SexpGrammar (Sexp :- t) ([FunctionDefinition] :- t)
funDeclList = sexpIso

caseAltPattern :: Grammar SexpGrammar (Sexp :- t) (CaseAltPattern :- t)
caseAltPattern = match
  $ With (\cconstant -> cconstant . clir_constant)
  $ With (\cconstructor -> cconstructor . clir_constructorApp)
  $ With (\cdefault -> cdefault . sym "default")
  $ End

caseAltExpr :: Grammar SexpGrammar (Sexp :- t) (CaseAltExpr :- t)
caseAltExpr = match
  $ With (\casealt -> casealt . list (el caseAltPattern >>> el sexpIso))
  $ End

instance SexpIso CaseAltExpr where
  sexpIso = caseAltExpr

caseAltExprList :: Grammar SexpGrammar (Sexp :- t) ([CaseAltExpr] :- t)
caseAltExprList = sexpIso

instance SexpIso GeneralExpression where
  sexpIso = match
    $ With (\be -> be . sexpIso)
    $ With (\let_ -> let_ . (list (el (sym "let")
                                   >>>
                                   el typedVarList
                                   >>>
                                   el sexpIso -- BindingExpression
                                   >>>
                                   el sexpIso -- GeneralExpression
                                  )))
    $ With (\letfun -> letfun . (list (el (sym "letfun")
                                       >>>
                                       el funDeclList
                                       >>>
                                       el sexpIso
                                      )))
    $ With (\case_ -> case_ . (list (el (sym "case") >>> 
                                     el sexpIso >>> 
                                     el caseAltExprList)))
    $ End

instance SexpIso TopLevelDef where
  sexpIso = match
    $ With (\fdef -> fdef . coproduct [ list (el (sym "define") >>>
                                              el symbol'      >>>
                                              el typedVarList >>>
                                              el typedVarList >>>
                                              el contracts    >>>
                                              el sexpIso)
                                      , list (el symbol'      >>>
                                              el typedVarList >>>
                                              el typedVarList >>>
                                              push []         >>>
                                              el sexpIso)
                                      ])
    $ With (\other -> other . (list (el (sym "verification-unit") >>>
                                     el string'  >>>
                                     props (
                                        Kw "sources" .: string' >>>
                                        Kw "uses"    .: string' >>>
                                        Kw "documentation" .: string')))) 
    $ End


-----------------------------------------------------------------------------------------
-- Parser for contracts. All are ignored except the preconditions and postconditions
-----------------------------------------------------------------------------------------


contracts :: Grammar SexpGrammar (Sexp :- t) ([Contract] :- t)
contracts = (list ((el (sym "declare")) >>>
                    rest sexpIso))

--contractList :: Grammar SexpGrammar (Sexp :- t) ([Contract] :- t)
--contractList = sexpIso


instance SexpIso Contract where
   sexpIso = match 
      $ With (\speccontract -> speccontract . (list (el (sym "assertion") >>>
                                                     rest sexpIso )))
      $ With (\othercontract -> othercontract . sym "other")
      $ End

--specList :: Grammar SexpGrammar (Sexp :- t) ([Spec] :- t)
--specList = sexpIso
 
instance SexpIso Spec where
   sexpIso = match
      $ With (\precd -> precd . (list (el (sym "precd") >>>
                                       el sexpIso)))
      $ With (\postcd -> postcd . (list (el (sym "postcd") >>>
                                         el sexpIso)))
      $ With (\otherassertion -> otherassertion . sexpIso)
      $ End

instance SexpIso Assertion where
    sexpIso = match
      $ With (\true  -> true . sym "true")
      $ With (\false -> false . sym "false")
      $ With (\not -> not . (list (el (sym "not") >>> 
                                   el sexpIso)))
      $ With (\op -> op . (list (el (sym "and") >>> 
                                 rest sexpIso)))
      $ With (\op -> op . (list (el (sym "or") >>> 
                                 rest sexpIso)))
      $ With (\op -> op . (list (el (sym "->") >>> 
                                 rest sexpIso)))
      $ With (\op -> op . (list (el (sym "<->") >>> 
                                 rest sexpIso)))
      $ With (\aplic -> aplic . (list (el (sym "@") >>>
                                       el symbol' >>>
                                       rest sexpIso)))
      $ With (\forall_ -> forall_ . (list (el (sym "forall") >>>
                                           el typedVarList  >>>
                                           el sexpIso ))) 
      $ With (\exists_ -> exists_ . (list (el (sym "exists") >>>
                                           el typedVarList  >>>
                                           el sexpIso ))) 
      $ End


