module Main where

import Text.ParserCombinators.Parsec    
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Data.Time.Calendar
import Data.Char (isSpace)
import System.IO

-- Put your favorite file name here
fname = "fxt.ledger"

main = do res <- parseFromFile journal fname
          case res of
            Left err -> do putStr "parse error at " ; print err
            Right rs -> mapM_ (putStrLn . show) rs
            -- I use mapM_ to print one record per line; otherwise, calling
            -- show of a list will put everything on the same line.

type Entry = String
type Directive = String
type Transaction = String
type Code = String
type Payee = String
type Amount = String
type Commodity = String
type Quantity = String
type Annotation = String
type Account = [String]
type Date = String

journal :: Parser [Entry]
journal = do xs <- many (   entry
                        <|> directive
                        <|> comment
                        <|> emptyLine )
             eof
             return [x | Just x <- xs]

-- WARNING: this has to be the last production or it will not parse
-- The reason is that the parser could choose this production by matching
-- empty and it will not be able to backtrack once it discovers empty is not
-- followed by a newline.
emptyLine :: Parser (Maybe Entry)
emptyLine = do many (oneOf " \t")
               newline
               return Nothing

comment :: Parser (Maybe Entry)
comment = do oneOf ";*hb"
             skipMany (satisfy (/= '\n'))
             newline
             return Nothing

directive :: Parser (Maybe Entry)
directive = do (oneOf "!*" >> wordDirective) <|> charDirective
               newline
               return Nothing

wordDirective :: Parser Directive
wordDirective = (string "include" >> longText)
            <|> (string "account" >> longText)
            <|> (string "end")
            <|> (string "alias" >> identifier >> char '=' >> longText)
            <|> (string "def" >> longText)

-- WARNING: should this really include trailing white space?
longText :: Parser String
longText = many1 (satisfy (/= '\n'))

-- WARNING: not sure about this one
identifier :: Parser String
identifier = many1 (satisfy (not . isSpace))

charDirective :: Parser Directive
charDirective = do oneOf "iIoO"
                   date
                   time
                   longText
                   return "i|I|o|O directive"
            <|> do char 'D'
                   amount
                   return "D directive"
            <|> do char 'A'
                   longText
                   return "A directive"
            <|> do char 'C'
                   commodity
                   char '='
                   amount
                   return "C directive"
            <|> do char 'P'
                   date
                   time
                   commodity
                   amount
                   return "P directive"
            <|> do char 'N'
                   commodity
                   return "N directive"
            <|> do char 'Y'
                   count 4 digit
                   return "Y directive"
            <|> do string "--"
                   identifier
                   longText
                   return "-- directive"

date :: Parser Date
date = do count 4 digit
          dateSep
          count 2 digit
          dateSep
          count 2 digit
          return "date"

dateSep :: Parser ()
dateSep = do oneOf "/-."
             return ()

time :: Parser ()
time = do count 2 digit
          char ':'
          count 2 digit
          char ':'
          count 2 digit
          return ()

commodity = identifier

-- WARNING: I need to define amount correctly
amount = identifier

entry :: Parser (Maybe Entry)
entry = do e <- plainEntry
           return (Just e)

plainEntry :: Parser Entry
plainEntry = do date
                optional (char '=' >> date)
                optional status
                optional code
                fullString
                optional note
                newline
                many1 transaction
                return "plain entry"

status :: Parser Char
status = oneOf "*!"

code :: Parser String
code = between (char '(') (char ')') (many1 (noneOf ")\n"))

note :: Parser String
note = do char ';'
          cs <- longText
          return cs

-- WARNING: this is not defined in grammar.y
fullString :: Parser String
fullString = many1 (noneOf ";\n")

transaction :: Parser Transaction
transaction = do many1 (oneOf " \t")
                 optional status
                 account
                 -- missing values_opt from grammar.y
                 optional note
                 newline
                 return "transaction"

account :: Parser String
account = accountName
      <|> between (char '(') (char ')') accountName
      <|> between (char '[') (char ']') accountName

accountName :: Parser String
accountName = do ns <- sepBy1 accountName' (char ':')
                 return (concat ns)

accountName' :: Parser String
accountName' = many1 (noneOf ")]:;\n")
