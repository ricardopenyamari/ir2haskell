-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Ricardo Peña, January 2017               
-- License     :  LGPL
--
-- Maintainer  :  ricardo@sip.ucm.es
-- Stability   :  provisional
-- Portability :  portable
--
-- The CAVI-ART CLIR-to-Haskell transformer

-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}


module Main where

import Language.Clir
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as TL
import qualified Data.Scientific as SC
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text,unpack)
import qualified Language.Sexp as Sexp
import Language.SexpGrammar (parseSexp, SexpIso(..), SexpG(..), decode)
import System.Environment
import Debug.Trace
import Data.Char
import qualified Text.PrettyPrint.Mainland as D
import Lib
------------------------------------------------------------------------------------------
-- First, we extend the usual Sexp parser in order to parse a sequence of S-expressions,
-- rather than a single one
------------------------------------------------------------------------------------------

decodes :: SexpIso a => Sexp.Position -> TL.Text -> Either String [a]
decodes p = decodesWith p sexpIso

decodesWith :: Sexp.Position -> SexpG a -> TL.Text -> Either String [a]
decodesWith p g input =
  Sexp.parseSexps' p input >>= mapM (parseSexp g)


------------------------------------------------------------------------------------------
-- This is the main program. It reads the CLIR file, and parses it as a list of
-- top-level IR definitions
------------------------------------------------------------------------------------------

main :: IO ()
main = do putStrLn "\nThis is the CAVI-ART CLIR-to-Haskell transformer tool\n"
          a <- getArgs             -- the input file name is a program argument
          let filename = head a
          let posIni   = Sexp.Position filename 1 0
          s <- B.readFile filename
          let bltotextl = TL.fromStrict . decodeUtf8 . B.toStrict
          let parsing = decodes posIni $ bltotextl s :: Either String [TopLevelDef]
          case parsing of
             Left  error -> putStrLn error
             Right defs  -> do let codeDoc = haskellcode defs
                               D.putDocLn codeDoc
                               putStr "\n"





------------------------------------------------------------------------------------------
-- Pretty-printing of the IR code section
------------------------------------------------------------------------------------------
{-
     We remind the AST definition 

type FunName = String
data TypedVar = TypedVar String ClirType

data TopLevelDef = TopFunDef FunName [TypedVar] [TypedVar] [Contract] GeneralExpression
                 | OtherInfo String String String String 
data FunctionDefinition = FunDef FunName [TypedVar] [TypedVar] [Contract] GeneralExpression
data GeneralExpression = Binding BindingExpression
                       | Let [TypedVar] BindingExpression GeneralExpression
                       | LetFun [FunctionDefinition] GeneralExpression
                       | Case AtomicExpression [CaseAltExpr]
data CaseAltExpr = CaseAlt CaseAltPattern GeneralExpression
data CaseAltPattern = CConstant ConstValue ClirType
                    | CConstructor DataConstructor [AtomicExpression]
                    | CDefault
data BindingExpression = AtomE AtomicExpression
                       | FunA FunName [AtomicExpression]
                       | ConstrA DataConstructor [AtomicExpression]
                       | Tuple [AtomicExpression]
data AtomicExpression  = Var VarName
                       | Const ConstValue ClirType
data ConstValue = ConstString String
                | ConstInt Int
                | ConstNumber Scientific
                | ConstBool Bool
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
type PredName = String
-}

ind = 2  -- indentation step

haskellcode :: [TopLevelDef] -> D.Doc
haskellcode topdefs = D.stack $ map topLevelDef topdefs

topLevelDef  :: TopLevelDef -> D.Doc
topLevelDef (TopFunDef fn args ress contracts exp) = precHeader D.</> D.indent ind prec D.</> 
                (funDef fn args ress contracts exp) D.</> postcHeader D.</> D.indent ind postc
    where (prec,postc) = contract contracts
          precHeader = D.text "uutPrec" D.<+> (D.spread $ map typedVar args) D.<+> D.equals
          postcHeader = D.text "uutPost" D.<+> (D.spread $ map typedVar args) D.<+> (D.spread $ map typedVar ress) D.<+> D.equals
topLevelDef (OtherInfo s1 s2 s3 s4)                = D.empty

contract :: [Contract] -> (D.Doc,D.Doc)
contract [] = (D.empty, D.empty)
contract [SpecContract xs] = (assertion (And pre),assertion (And post))
  where (pre,post) = specContracts xs [] []
contract ((SpecContract xs):cs) = (D.text "And" D.</> D.indent ind (D.parens (assertion (And pre1)) D.</> D.parens pre2), 
                                  D.text "And" D.</> D.indent ind (D.parens (assertion (And post1)) D.</> D.parens post2))
  where (pre2,post2) = contract cs
        (pre1,post1) = specContracts xs [] []

specContracts xs pre post = case xs of
  [] -> (pre, post)
  (PreCD a):ys  -> specContracts ys (a:pre) post
  (PostCD a):ys -> specContracts ys pre (a:post)


funDef fn args ress contracts exp  = header D.</> D.indent ind body

  where header = D.text fn D.<+> 
                 (D.spread $ map typedVar args) D.<+>
                 D.equals
        body   = generalExp exp

typedVar :: TypedVar -> D.Doc
typedVar (TypedVar var clirType) = D.text var

generalExp  :: GeneralExpression -> D.Doc

generalExp (Binding e) = bindingExp e

generalExp (Let tvs e1 e2) = D.text "let" D.<+> 
                             (case tvs of
                                [var] -> typedVar var
                                _     -> D.tuple $ map typedVar tvs) D.<+>      
                             D.equals D.<+>
                             bindingExp e1 D.<+>
                             D.text "in" D.</> 
                             (D.indent ind $ generalExp e2)

generalExp (LetFun fs e) = generalExp e D.</>
                           (D.indent ind $ 
                             (D.text "where" D.</> 
                                (D.indent ind $ 
                                  (D.stack $ map innerLevelDef fs))))

generalExp (Case e alts) = D.text "case" D.<+>
                           atomicExp e D.<+>
                           D.text "of" D.</>
                           D.indent ind (D.stack $ map altExp alts)

altExp :: CaseAltExpr -> D.Doc
altExp (CaseAlt pat e) = altPattern pat D.<+>
                         D.text "->" D.<+>
                         generalExp e

altPattern  :: CaseAltPattern -> D.Doc
altPattern (CConstant  v clirType) = constValue v
altPattern (CConstructor cn aes)   = D.text cn D.<+> (D.spread $ map atomicExp aes)
altPattern (CDefault )             = D.text "_" 

innerLevelDef :: FunctionDefinition -> D.Doc
innerLevelDef (FunDef fn args ress contracts exp) = funDef fn args ress contracts exp
                               
bindingExp :: BindingExpression -> D.Doc
bindingExp (AtomE ae)       = atomicExp ae 
bindingExp (FunA fn aes)    = (if isAlpha $ head fn 
                               then D.text fn
                               else D.parens $ D.text fn) D.<+> 
                              (D.spread $ map atomicExp aes)
bindingExp (ConstrA cn aes) = D.text cn D.<+> (D.spread $ map atomicExp aes)
bindingExp (Tuple aes)      = D.tuple $ map atomicExp aes

atomicExp :: AtomicExpression -> D.Doc
atomicExp (Var vn)  = D.text vn
atomicExp (Const v clirType) = constValue v

constValue :: ConstValue -> D.Doc
constValue (ConstString s) = D.dquotes $ D.text s
constValue (ConstInt i)    = D.int i
constValue (ConstNumber n) = D.float (SC.toRealFloat n)
constValue (ConstBool b)   = D.bool b

bindingExpTerm :: BindingExpression -> D.Doc
bindingExpTerm (AtomE aes)      = atomicExpTerm aes 
bindingExpTerm (FunA fn aes)    = foldl f (D.parens $ D.text "TConst" D.<+> (if isAlpha $ head fn then D.text fn else D.parens $ D.text fn)) aes
  where f fn a = D.text "Aplic" D.<+> D.parens fn D.<+> D.parens (atomicExpTerm a)
bindingExpTerm (ConstrA cn aes) = D.text "TConst" D.<+> (D.parens $ D.text cn D.<+> (D.spread $ map atomicExpTerm aes))
bindingExpTerm (Tuple aes)      = (if (length aes)==2 then D.text "Tuple2" else D.text "Tuple3") D.<+> D.spread (map atomicExpTerm aes)

atomicExpTerm :: AtomicExpression -> D.Doc
atomicExpTerm (Var vn)           = D.text "TVar" D.<+> D.text vn
atomicExpTerm (Const v clirType) = D.text "TConst" D.<+> constValue v

assertion :: Assertion -> D.Doc
assertion (ATrue)    = D.text "FTerm (TConst True)"
assertion (AFalse)   = D.text "FTerm (TConst False)"
assertion (Not a)    = D.text "Not" D.<+> D.parens (assertion a)
assertion (Aplic name xs)
  |name=="="      = D.text "FTerm" D.<+> foldl f (D.parens $ D.text "TConst" D.<+> D.text "(==)") xs
  |name=="[_.._]" = D.text "FTerm" D.<+> foldl f (D.parens $ D.text "TConst" D.<+> D.text "subsec") xs
  |name=="[_..]"  = D.text "FTerm" D.<+> foldl f (D.parens $ D.text "TConst" D.<+> D.text "subsec_ini") xs
  |name=="[.._]"  = D.text "FTerm" D.<+> foldl f (D.parens $ D.text "TConst" D.<+> D.text "subsec_fin") xs
  |otherwise      = D.text "FTerm" D.<+> foldl f (D.parens $ D.text "TConst" D.<+> (if isAlpha $ head name 
                    then D.text name else D.parens $ D.text name)) xs
  where f fn x = D.parens $ D.text "Aplic" D.<+> fn D.<+> D.parens (bindingExpTerm x)

assertion (And xs)   = f xs
  where f xs = case xs of
               [x]    -> assertion x
               (x:xs) -> D.text "And" D.</> D.indent ind (D.parens (assertion x) D.</> D.parens (f xs))

assertion (Or xs)    = f xs
  where f xs = case xs of
               [x]    -> assertion x
               (x:xs) -> D.text "Or" D.</> D.indent ind (D.parens (assertion x) D.</> D.parens (f xs))

assertion (Equiv xs) = f xs
  where f xs = case xs of 
               [x1,x2]    -> D.text "Equiv" D.</> D.indent ind (D.parens (assertion x1) D.</> D.parens (assertion x2))
               (x1:x2:xs) -> D.text "And" D.</> D.indent ind (D.parens (f [x1,x2]) D.</> D.parens (f (x2:xs)))

assertion (Imp xs) = D.text "Imp" D.</> D.indent ind (D.parens (f (init xs)) D.</> D.parens (assertion (last xs)))
  where f xs = case xs of
               [x]    -> assertion x
               (x:xs) -> D.text "And" D.</> D.indent ind (D.parens (assertion x) D.</> D.parens (f xs))

assertion (Forall [TypedVar i (SimpleType t)] (Imp xs)) = D.text "Forall" D.</> D.indent ind (D.parens 
  (if (unpack t)=="int" then guardInt i xs [] [] else guards i (head xs)) D.</> 
  D.parens (D.text "\\" D.<> D.text i D.<+> D.text "->" D.<+> D.parens (assertion (Imp xs))))
assertion (Forall [TypedVar i (SimpleType ti), TypedVar j (SimpleType tj)] (Imp xs)) = if (unpack ti)=="int"&&(unpack tj)=="int" 
  then D.text "Forall" D.</> D.indent ind (D.parens (guardIntTuple i j xs [] [] [] [] "none") D.</> 
       D.parens (D.text "\\" D.<> D.tuple [D.string i,D.string j] D.<+> D.text "->" D.<+> D.parens (assertion (Imp xs))))
  else D.empty
-- assertion (Forall [TypedVar a _] (Imp [g,q])) = D.parens $ D.text "Forall" D.<+> (guards a g) D.<+> 
                                                -- D.parens (D.text a D.<+> D.text "->" D.<+> (assertion (Imp [g,q])))

assertion (Exists [TypedVar i (SimpleType t)] (Imp xs)) = D.text "Exists" D.</> D.indent ind (D.parens 
  (if (unpack t)=="int" then guardInt i xs [] [] else guards i (head xs)) D.</> 
  D.parens (D.text "\\" D.<> D.text i D.<+> D.text "->" D.<+> D.parens (assertion (Imp xs))))
assertion (Exists [TypedVar i (SimpleType ti), TypedVar j (SimpleType tj)] (Imp xs)) = if (unpack ti)=="int"&&(unpack tj)=="int" 
  then D.text "Exists" D.</> D.indent ind (D.parens (guardIntTuple i j xs [] [] [] [] "none") D.</> 
       D.parens (D.text "\\" D.<> D.tuple [D.text i,D.text j] D.<+> D.text "->" D.<+> D.parens (assertion (Imp xs))))
  else D.empty
-- assertion (Exists [TypedVar a _] (Imp [g,q])) = D.parens $ D.text "Exists" D.<+> (guards a g) D.<+> 
                                                -- D.parens (D.text a D.<+> D.text "->" D.<+> (assertion (Imp [g,q])))

guardInt::String -> [Assertion] -> [D.Doc] -> [D.Doc] -> D.Doc
guardInt i [Imp xs] mins maxs = guardInt i xs mins maxs
guardInt _ [_] mins maxs = D.text "GuardInt" D.<+> docmins D.<+> docmaxs
  where docmins = if (length mins)==1 then (head mins) else D.parens (D.text "Aplic (TConst maximum) (TVar" D.<+> D.list mins D.<> D.rparen)
        docmaxs = if (length maxs)==1 then (head maxs) else D.parens (D.text "Aplic (TConst minimum) (TVar" D.<+> D.list maxs D.<> D.rparen)
guardInt i ((Aplic op [AtomE (Var v1), AtomE (Var v2)]):gs) mins maxs = 
  if v1==i then (let (min,max)=(calculateMinMax op (AtomE (Var v2))) in (guardInt i gs (min++mins) (max++maxs)))
  else if v2==i then (let (min,max)=(calculateMinMax (inverseOp op) (AtomE (Var v1))) in (guardInt i gs (min++mins) (max++maxs)))
  else (guardInt i gs mins maxs)
guardInt i ((Aplic op [AtomE (Var v), exp]):gs) mins maxs = 
  if v==i then (let (min,max)=(calculateMinMax op exp) in (guardInt i gs (min++mins) (max++maxs)))
  else (guardInt i gs mins maxs)
guardInt i ((Aplic op [exp, AtomE (Var v)]):gs) mins maxs = 
  if v==i then (let (min,max)=(calculateMinMax (inverseOp op) exp) in (guardInt i gs (min++mins) (max++maxs)))
  else (guardInt i gs mins maxs)

guardIntTuple::String -> String -> [Assertion] -> [D.Doc] -> [D.Doc] -> [D.Doc] -> [D.Doc] -> String -> D.Doc
guardIntTuple i j [Imp xs] mins1 maxs1 mins2 maxs2 rel = guardIntTuple i j xs mins1 maxs1 mins2 maxs2 rel
guardIntTuple _ _ [_] mins1 maxs1 mins2 maxs2 rel = D.text "GuardIntTuple" D.</> D.indent ind
  (D.parens (D.text "Tuple2" D.<+> docminsi D.<+> docminsj) D.</> D.parens (D.text "Tuple2" D.<+> docmaxsi D.<+> docmaxsj))
  where (minsi,maxsj) = if rel=="gt" then ((mins1++mins2),(maxs1++maxs2)) else (mins1,maxs2)
        (maxsi,minsj) = if rel=="lt" then ((maxs1++maxs2),(mins1++mins2)) else (maxs1,mins2)
        docminsi = if (length minsi)==1 then (head minsi) else D.parens (D.text "Aplic (TConst maximum) (TVar" D.<+> D.list minsi D.<> D.rparen)
        docminsj = if (length minsj)==1 then (head minsj) else D.parens (D.text "Aplic (TConst maximum) (TVar" D.<+> D.list minsj D.<> D.rparen)
        docmaxsi = if (length maxsi)==1 then (head maxsi) else D.parens (D.text "Aplic (TConst minimum) (TVar" D.<+> D.list maxsi D.<> D.rparen)
        docmaxsj = if (length maxsj)==1 then (head maxsj) else D.parens (D.text "Aplic (TConst minimum) (TVar" D.<+> D.list maxsj D.<> D.rparen)
guardIntTuple i j ((Aplic op [AtomE (Var v1), AtomE (Var v2)]):gs) mins1 maxs1 mins2 maxs2 rel =
  if (v1==i&&v2==j&&(op=="<="||op=="<"))||(v1==j&&v2==i&&(op==">="||op==">"))
     then (guardIntTuple i j gs mins1 maxs1 mins2 maxs2 "lt")
  else if (v1==i&&v2==j&&(op==">="||op==">"))||(v1==j&&v2==i&&(op=="<="||op=="<"))
     then (guardIntTuple i j gs mins1 maxs1 mins2 maxs2 "gt")
  else if v1==i then (let (min,max)=(calculateMinMax op (AtomE (Var v2))) in (guardIntTuple i j gs (min++mins1) (max++maxs1) mins2 maxs2 rel))
  else if v1==j then (let (min,max)=(calculateMinMax op (AtomE (Var v2))) in (guardIntTuple i j gs mins1 maxs1 (min++mins2) (max++maxs2) rel))
  else if v2==i then (let (min,max)=(calculateMinMax (inverseOp op) (AtomE (Var v1))) in (guardIntTuple i j gs (min++mins1) (max++maxs1) mins2 maxs2 rel))
  else if v2==j then (let (min,max)=(calculateMinMax (inverseOp op) (AtomE (Var v1))) in (guardIntTuple i j gs mins1 maxs1 (min++mins2) (max++maxs2) rel))
  else (guardIntTuple i j gs mins1 maxs1 mins2 maxs2 rel)
guardIntTuple i j ((Aplic op [AtomE (Var v), exp]):gs) mins1 maxs1 mins2 maxs2 rel =
  if v==i then (let (min,max)=(calculateMinMax op exp) in (guardIntTuple i j gs (min++mins1) (max++maxs1) mins2 maxs2 rel))
  else if v==j then (let (min,max)=(calculateMinMax op exp) in (guardIntTuple i j gs mins1 maxs1 (min++mins2) (max++maxs2) rel))
  else (guardIntTuple i j gs mins1 maxs1 mins2 maxs2 rel)
guardIntTuple i j ((Aplic op [exp, AtomE (Var v)]):gs) mins1 maxs1 mins2 maxs2 rel =
  if v==i then (let (min,max)=(calculateMinMax (inverseOp op) exp) in (guardIntTuple i j gs (min++mins1) (max++maxs1) mins2 maxs2 rel))
  else if v==j then (let (min,max)=(calculateMinMax (inverseOp op) exp) in (guardIntTuple i j gs mins1 maxs1 (min++mins2) (max++maxs2) rel))
  else (guardIntTuple i j gs mins1 maxs1 mins2 maxs2 rel)
  
inverseOp:: String -> String
inverseOp op = case op of
  ">=" -> "<="
  ">"  -> "<"
  "<=" -> ">="
  "<"  -> ">"

calculateMinMax:: String -> BindingExpression -> ([D.Doc],[D.Doc])
calculateMinMax op exp = case op of
  ">=" -> ([D.parens $ bindingExpTerm exp],[])
  ">"  -> ([D.parens $ D.text "Aplic" D.<+> 
           (D.parens $ D.text "Aplic (TConst (+))" D.<+> bindingExpTerm exp) D.<+> 
           (D.parens $ D.text "TConst" D.<+> D.int 1)],[])
  "<=" -> ([],[D.parens $ bindingExpTerm exp])
  "<"  -> ([],[D.parens $ D.text "Aplic" D.<+> 
              (D.parens $ D.text "Aplic (TConst (-))" D.<+> bindingExpTerm exp) D.<+> 
              (D.parens $ D.text "TConst" D.<+> D.int 1)])

--Set SEQ y bag
guards a (Aplic "mem" [AtomE (Var v), exp]) = D.parens $ D.text "GuardSeq" D.<+> D.parens (bindingExp exp)