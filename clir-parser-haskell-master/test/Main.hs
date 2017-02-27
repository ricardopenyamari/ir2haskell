{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8)
import Language.SexpGrammar (decode, encode)
import Language.Clir
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not a Right"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [unitTests]

unitTests = testGroup "Unit tests" [test1, test2, test3, test4]

t = encode $ Const (ConstString "5") (SimpleType "int")

test1 = testCase "Check simple constant identity" $
  do
    assertEqual "Isomorphism holds" x (fromRight decoded)
      where x = Const (ConstString "5") (SimpleType "int")
            bltotextl = TL.fromStrict . decodeUtf8 . BL.toStrict

            encoded :: TL.Text
            encoded = bltotextl $ fromRight $ encode x
            decoded :: Either String AtomicExpression
            decoded = decode encoded


test2 = testCase "Check let with function application" $
  do
    decoded <- return $ (fromRight $ decode x :: GeneralExpression)
    assertEqual "encode-decode is idempotent" decoded expected
      where x = "(let ((x int)) (@ f (the bool false)) x)"
            expected = Let
                       [TypedVar "x" (SimpleType "int")]
                       (FunA "f" [Const (ConstBool False) (SimpleType "bool")])
                       (Binding (AtomE (Var "x")))


t3  :: Either String AtomicExpression
t3 = decode "(the (array 'a) \"x\")"

test3 = testCase "Check that a type variable is instantiated correctly" $ do
  case t3 of
    Left msg -> assertFailure $ "Failed on parsing: " ++ msg
    Right t ->
      case t of
        Const _ (CompoundType [_, TypeVar a]) -> return ()
        _ -> assertFailure "Detected something other than a compoundtype with a typevar"


t4 :: Either String GeneralExpression
t4 = decode "(case x ((the int 1) (the int 1)) (default x))"

test4 = testCase "Check a case expression with default parameter" $ do
    decoded <- return $ (fromRight $ decode x :: GeneralExpression)
    assertEqual "Case expression matches the in-memory generated AST" decoded expected
      where x = "(case x ((the int 1) (the int 1)) (default x))"
            expected = Case (Var "x")
                       [ CaseAlt
                         (CConstant (ConstInt 1) (SimpleType "int"))
                         (Binding
                          (AtomE
                           (Const (ConstInt 1) (SimpleType "int"))))
                       , CaseAlt
                         CDefault
                         (Binding
                          (AtomE
                           (Var "x")))]


