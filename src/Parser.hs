module Parser where

import Numeric
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
 
data LispVal =
    Nil
    | Atom String
    | Number Integer
    | String String
    | Bool Bool
    | Char Char
    | List [LispVal]
    | DottedList [LispVal] LispVal
    deriving (Eq)

instance Show LispVal where
    show (Nil)             = "Nil"
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name)       = name
    show (Number contents) = show contents
    show (Bool True)       = "#t"
    show (Bool False)      = "#f"
    show (Char x)          = show x
    show (List x)          = "(" ++ stringify x ++ ")"
    show (DottedList x x') = "(" ++ stringify x ++ " . " ++ show x' ++ ")"

stringify :: [LispVal] -> String
stringify = unwords . map show

readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value"

--------------------------------------------------------------------------------
-- Parsers
--
-- The Parser type is a monad. The hidden (extra) information in Parser is the
-- info about the position in the input stream, backtracking data structures,
-- etc...
--
-- Parsec parsers consume the input when they succeed. Be extra
-- careful when parsing expressions with common substrings. Ex:
--
--    "(1 3)"
--    "(1 3 . 2)"
--
-- In both expressions "(1 3" is a common substring. If the first parser
-- consumes "(1 3" and fails for some reason then the second one cannot parse
-- the list because it cannot backtrack.
--
--
-- Some functions provided by the Parsec library. Many functions that take
-- other parsers have a version with a 1 at the end. This means apply the
-- parser passed as argument ONE or more times, as opposed to ZERO or more
-- times.
--
--    (p <|> q) first applies the parser p and returns its value if it
--    succeeds. If p fails without consuming any input, parser q is tried.
--
--    (p <?> msg) behaves as parser p, but whenever the parser p fails without
--    consuming any input, it replaces expect error messages with the expect
--    error message msg. For example, if the following parser fails, an error
--    message saying "Expected expression" will be printed:
--      expr       = letExpr <|> identifier <?> "expression"
--      letExpr    = do{ string "let"; ... }
--      identifier = many1 letter
--
--    (try p) behaves like parser p, except that it pretends that it hasn't
--    consumed any input when an error occurs.  This combinator is used
--    whenever arbitrary look ahead is needed.  Since it pretends that it
--    hasn't consumed any input when p fails, the (<|>) combinator will try its
--    second alternative even when the first parser failed while consuming
--    input.
--
--    (many p) applies the parser p zero or more times. Returns a list of the
--    returned values of p.
--
--    (skipMany p) applies the parser p zero or more times, skipping its
--    result.
--
--    (sepBy p sep) parses zero or more occurrences of p, separated by sep.
--    Returns a list of values returned by p.
--
--    (endBy p sep) parses zero or more occurrences of p, seperated and ended
--    by sep. Returns a list of values returned by p.
--
--    (sepEndBy p sep) parses zero or more occurrences of p, separated and op-
--    tionally ended by sep, ie. haskell style statements. Returns a list of
--    values returned by p.
--
--    (between open close p) parses open, followed by p and close. Returns the
--    value returned by p.
--
--    (option x p) tries to apply parser p. If p fails without consuming input,
--    it returns the value x, otherwise the value returned by p.
--
--    (oneOf cs) succeeds if the current character is in the supplied list of
--    characters cs. Returns the parsed character.
--
--    (noneOf cs) succeeds if the current character is not in the supplied list
--    of characters cs. Returns the parsed character.
--
--    (char c) parses a single character c. Returns the parsed character.
--
--    (string s) parses a sequence of characters given by s. Returns the parsed
--    string.
--------------------------------------------------------------------------------

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LispVal
parseExpr =
    parseAtom <|>
    parseString <|>
    parseNumber <|>
    parseBool <|>
    parseChar <|>
    parseQuoted <|>
    parseList

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom (first:rest)

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> noneOf "\"\\")
    char '"'
    return $ String x

-- Read a backslash and return the escaped char
escapedChar :: Parser Char
escapedChar = do
    char '\\'
    x <- oneOf "\"\\nrt"
    return $ case x of
        '\\' -> x
        '\"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseNumber :: Parser LispVal
parseNumber = parseDec1 <|> parseDec2 <|> parseHex <|> parseOctal

parseDec1 :: Parser LispVal
parseDec1 = many1 digit >>= return . Number . read

parseDec2 :: Parser LispVal
parseDec2 = do
    try $ string "#d"
    many1 digit >>= return . Number . read

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    many1 hexDigit >>= return . Number . fst . (!!0) . readHex

parseOctal :: Parser LispVal
parseOctal = do
    try $ string "#o"
    many1 octDigit >>= return . Number . fst . (!!0) . readOct

parseBool :: Parser LispVal
parseBool =
    try (string "#t" >> return (Bool True)) <|>
    try (string "#f" >> return (Bool False))

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    x <- anyChar
    notFollowedBy $ noneOf " "
    return $ Char x

-- A quoted expression: 'expr
parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList =
    let beg = char '(' >> skipMany space in
    let end = skipMany space >> char ')' in
    between beg end parseList1

parseList1 :: Parser LispVal
parseList1 = do
    list <- sepEndBy parseExpr $ skipMany1 space
    datum <- option Nil $ char '.' >> skipMany1 space >> parseExpr
    return $ case datum of
        Nil -> List list
        val -> DottedList list val
