module Main (main) where

import Test.HUnit
import Expression (Expression(..))
import Tokenize (tokenize)
import Parser (expr, processCode)
import Evaluator (evaluate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types (InterpreterConfig(..))

main :: IO ()
main = runTestTTAndExit allTests

testConfig :: InterpreterConfig
testConfig = InterpreterConfig {
    maxSteps = 1000,
    debug = False,
    tracing = False,
    extensions = Set.empty,
    memoization = False
}

allTests :: Test
allTests = TestList [
    TestLabel "Tokenization" tokenizationTests,
    TestLabel "Parsing" parsingTests,
    TestLabel "Evaluation" evaluationTests,
    TestLabel "Definitions" definitionTests
  ]

tokenizationTests :: Test
tokenizationTests = TestList [
    "Simple variable" ~:
        tokenize "x" ~?= ["x"],

    "Lambda expression" ~:
        tokenize "λx. x" ~?= ["λ", "x", ".", "x"],

    "Application with parentheses" ~:
        tokenize "(f x)" ~?= ["(", "f", "x", ")"],

    "Let definition" ~:
        tokenize "let id = λx. x" ~?= ["let", "id", "=", "λ", "x", ".", "x"],

    "Nested expressions" ~:
        tokenize "(λx. (λy. (x y)))" ~?= ["(", "λ", "x", ".", "(", "λ", "y", ".", "(", "x", "y", ")", ")", ")"]
  ]

parsingTests :: Test
parsingTests = TestList [
    "Parse variable" ~:
        case expr ["x"] of
            Right (Var "x", []) -> True
            _ -> False
        ~?= True,

    "Parse lambda" ~:
        case expr ["λ", "x", ".", "x"] of
            Right (Lam "x" (Var "x"), []) -> True
            _ -> False
        ~?= True,

    "Parse application" ~:
        case expr ["(", "f", "x", ")"] of
            Right (App (Var "f") (Var "x"), []) -> True
            _ -> False
        ~?= True,

    "Parse nested lambda" ~:
        case expr ["λ", "x", ".", "λ", "y", ".", "x"] of
            Right (Lam "x" (Lam "y" (Var "x")), []) -> True
            _ -> False
        ~?= True
  ]

evaluationTests :: Test
evaluationTests = TestList [
    "Identity function with bound variable" ~:
        let id = Lam "x" (Var "x")
            testExpr = App id (Lam "y" (Var "y"))
            env = Map.empty
            usedDefs = Set.empty
            (result, _, _) = evaluate testConfig testExpr env usedDefs
        in result ~?= Lam "y" (Var "y"),

    "Basic substitution with bound variable" ~:
        let testExpr = App (Lam "x" (App (Var "x") (Var "x"))) (Lam "y" (Var "y"))
            env = Map.empty
            usedDefs = Set.empty
            (result, _, _) = evaluate testConfig testExpr env usedDefs
            expected = Lam "y" (Var "y")  -- Corrigido para a função identidade
        in result ~?= expected,

    "Multiple beta reduction with bound variables" ~:
        let k = Lam "x" (Lam "y" (Var "x"))
            testExpr = App (App k (Lam "a" (Var "a"))) (Lam "b" (Var "b"))
            env = Map.empty
            usedDefs = Set.empty
            (result, _, _) = evaluate testConfig testExpr env usedDefs
        in result ~?= Lam "a" (Var "a"),

    "Nested lambda evaluation with bound variable" ~:
        let testExpr = App (Lam "x" (Lam "y" (App (Var "x") (Var "y")))) (Lam "z" (Var "z"))
            env = Map.empty
            usedDefs = Set.empty
            (result, _, _) = evaluate testConfig testExpr env usedDefs
        in result ~?= Lam "y" (App (Lam "z" (Var "z")) (Var "y"))
    ]

definitionTests :: Test
definitionTests = TestList [
    "Simple definition processing" ~:
        case processCode "let id = λx. x" of
            Right (Lam "x" (Var "x"), env) ->
                Map.lookup "id" env == Just (Lam "x" (Var "x"))
            _ -> False
        ~?= True,

    "Multiple definitions processing" ~:
        case processCode "let id = λx. x\nlet const = λx. λy. x" of
            Right (_, env) ->
                Map.size env == 2
            _ -> False
        ~?= True,

    "Definition with application" ~:
        case processCode "let apply = λf. λx. (f x)\nlet id = λx. x\nlet test = λw. w\n(apply id test)" of
            Right (parsedExpr, env) ->
                let (result, _, _) = evaluate testConfig parsedExpr env Set.empty
                    expectedResult = Lam "w" (Var "w")  -- id aplicado a test deve resultar em test
                in result == expectedResult &&
                   Map.lookup "apply" env == Just (Lam "f" (Lam "x" (App (Var "f") (Var "x")))) &&
                   Map.lookup "id" env == Just (Lam "x" (Var "x"))
            _ -> False
        ~?= True
  ]
