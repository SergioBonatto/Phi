import Test.HUnit
import Expression (Expression(..))
import Tokenize (tokenize)
import Parser (parseExpr, processCode)
import Evaluator (evaluate)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = runTestTTAndExit allTests

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
        tokenize "let id = λx. x" ~?= ["let", "id", "=", "λ", "x", ".", "x"]
  ]

parsingTests :: Test
parsingTests = TestList [
    "Parse variable" ~:
        case parseExpr ["x"] of
            Right (Var "x", []) -> True
            _ -> False
        ~?= True,

    "Parse lambda" ~:
        case parseExpr ["λ", "x", ".", "x"] of
            Right (Lam "x" (Var "x"), []) -> True
            _ -> False
        ~?= True,

    "Parse application" ~:
        case parseExpr ["(", "f", "x", ")"] of
            Right (App (Var "f") (Var "x"), []) -> True
            _ -> False
        ~?= True
  ]

evaluationTests :: Test
evaluationTests = TestList [
    "Identity function" ~:
        let expr = App (Lam "x" (Var "x")) (Var "y")
            env = Map.empty
            usedDefs = Set.empty
            (result, _) = evaluate expr env usedDefs 1000
        in result ~?= Var "y",

    "Basic substitution" ~:
        let expr = App (Lam "x" (App (Var "x") (Var "x"))) (Var "y")
            env = Map.empty
            usedDefs = Set.empty
            (result, _) = evaluate expr env usedDefs 1000
        in result ~?= App (Var "y") (Var "y"),

    "Multiple beta reduction" ~:
        let expr = App (App (Lam "x" (Lam "y" (Var "x"))) (Var "a")) (Var "b")
            env = Map.empty
            usedDefs = Set.empty
            (result, _) = evaluate expr env usedDefs 1000
        in result ~?= Var "a"
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
        ~?= True
  ]
