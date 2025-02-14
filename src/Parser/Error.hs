module Error (
    Error(..),
    ParserError(..),
    EvalError(..),
    ConfigError(..),
    Position(..)
) where

-- | Representa a posição no código fonte
data Position = Position {
    line :: Int,
    column :: Int
} deriving (Show, Eq)

-- | Erros de parser
data ParserError
    = UnexpectedEndOfInput String           -- ^ Fim inesperado da entrada
    | UnexpectedToken String String         -- ^ Token encontrado vs esperado
    | UnclosedParenthesis String           -- ^ Parênteses não fechado
    | InvalidLambdaSyntax                   -- ^ Sintaxe inválida de lambda
    | EmptyExpression                       -- ^ Expressão vazia
    deriving (Show, Eq)

-- | Erros de avaliação
data EvalError
    = MaxStepsExceeded Int
    | UnboundVariable String
    | TypeError String
    | RuntimeError String
    deriving (Show, Eq)

-- | Erros de configuração
data ConfigError
    = InvalidStepLimit Int
    | InvalidLogLevel String
    | InvalidExtension String
    deriving (Show, Eq)

-- | Tipo principal de erro
data Error
    = ParserError ParserError
    | EvalError EvalError
    | ConfigError ConfigError
    deriving (Show, Eq)
