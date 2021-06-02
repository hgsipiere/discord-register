module Parser where

-- i think this whole file can be replaced
-- with about 12 characters of code less than
-- a line with a regular expression
-- the benefit of a parser's error messages
-- we never use because, interpreting parser combinator
-- error messages is not useful as the error
-- messages are why it can't continue not why
-- it doesnt end parsing

import Data.List (intercalate)
import qualified Data.Text.Lazy as TL (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, runParser, some, (<|>))
import Text.Megaparsec.Byte (space, string)
import Text.Megaparsec.Char as C (char, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as CL
import TextShow (showtl)

--import Db

-- parser for full name, the underscore
-- is to handle the edge case of a single name, full name
type ParserStr = Parsec Void String

preLexeme, lexeme :: ParserStr a -> ParserStr a
lexeme = CL.lexeme C.space
preLexeme = (*>) C.space

nameOrUnderscore', fullName :: ParserStr String
nameOrUnderscore' = some letterChar <|> (fmap pure . char $ '_')
fullName = do
  firstName <- preLexeme nameOrUnderscore'
  restNames <- lexeme . some . preLexeme $ nameOrUnderscore'
  pure . intercalate " " $ firstName : restNames

parseFullName content = showtl <$> (runParser fullName "" (TL.unpack content))
