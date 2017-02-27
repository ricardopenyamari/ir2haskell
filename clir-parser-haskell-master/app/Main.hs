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
import Data.Text (Text)
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
topLevelDef (TopFunDef fn args ress contracts exp) = funDef fn args ress contracts exp
topLevelDef (OtherInfo s1 s2 s3 s4)                = D.empty


funDef fn args ress _ exp  = header D.</> D.indent ind body

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

