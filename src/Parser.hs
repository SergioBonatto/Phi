module Parser (
    parseExpression,
    parseDefinition,
    parseProgram
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Combinators.Expr
import Types
import Error

type Parser = Parsec Void Text

parseExpression :: Text -> Either ParseError Expression
parseExpression = runParser expressionParser ""

expressionParser :: Parser Expression
expressionParser = makeExprParser termParser operatorTable

termParser :: Parser Expression
termParser = choice
    [ Var <$> identifier
    , Lam <$> (symbol "λ" *> identifier <* symbol ".") <*> expressionParser
    , parens expressionParser
    ]

operatorTable :: [[Operator Parser Expression]]
operatorTable =
    [ [ InfixL (App <$ space1) ]
    ]

identifier :: Parser Text
identifier = lexeme (T.pack <$> some alphaNumChar)

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
