module JournalParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Text.ParserCombinators.Parsec.Expr
import Data.Ratio
import Data.Char
import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Control.Monad (when)

import qualified Account as A

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellStyle {
    P.reservedNames   = ["buy", "sell", "split", "exchange", "n"]
  , P.reservedOpNames = ["+", "-", "*", "/"]
  })

whiteSpace    = P.whiteSpace lexer
lexeme        = P.lexeme lexer
colon         = P.colon lexer
decimal       = P.decimal lexer
identifier    = P.identifier lexer
parens        = P.parens lexer
reserved      = P.reserved lexer
reservedOp    = P.reservedOp lexer
symbol        = P.symbol lexer
stringLiteral = P.stringLiteral lexer


journalParser :: Parser [A.Account ()]
journalParser = do whiteSpace
                   as <- many activity
                   eof
                   return as

activity :: Parser (A.Account ())
activity = buy <|> sell <|> split <|> exchange

buy :: Parser (A.Account ())
buy = do reserved "buy"
         dat <- date
         sym <- asset
         qty <- quantity
         cst <- cost qty
         fail (show cst)
         return (A.buy dat sym qty cst)

sell :: Parser (A.Account ())
sell = do reserved "sell"
          dat <- date
          sym <- asset
          qty <- quantity
          pro <- proceeds qty
          return (A.sell dat sym qty pro)

split :: Parser (A.Account ())
split = do reserved "split"
           dat <- date
           sym <- asset
           rat <- splitRatio
           return (A.split dat sym rat)

exchange :: Parser (A.Account ())
exchange = do reserved "exchange"
              dat <- date
              sym <- asset
              qty <- quantity
              sym' <- asset
              qty' <- quantity
              return (A.exchange dat sym qty sym' qty')

date :: Parser Day
date = lexeme date' <?> "date as 'yyyy/mm/dd'"

date' :: Parser Day
date' = do sy <- count 4 (digit <?> "year as 'yyyy'")
           sep <- oneOf "/:.-"  <?>
                  "date field separator (one of '/', ':', '.', or '-')"
           m <- oneOrTwoDigits <?> "month as 'mm'"
           char sep <?> ("date field separator " ++ show sep)
           d <- oneOrTwoDigits <?> "day as 'dd'"
           let y = read sy
           when (m > 12 || d > (julianMonthLength y m))
               (fail ("invalid date: " ++ sy ++ [sep] ++ show m ++ [sep] ++ show d))
           return (fromGregorian y m d)

oneOrTwoDigits :: Parser Int
oneOrTwoDigits = do d <- digit
                    secondDigit (digitToInt d)

secondDigit :: Int -> Parser Int
secondDigit n =   do d <- digit <?> ""
                     return (10 * n + digitToInt d)
              <|> return n

asset :: Parser A.Symbol
asset = identifier <|> stringLiteral <?> "symbol or asset description"

splitRatio :: Parser Rational
splitRatio =   do n <- lexeme decimal
                  colon <?> "split ratio separator ':'"
                  d <- lexeme decimal
                  when (n <= 0 || d <= 0)
                      (fail ("invalid split ratio: " ++ show n ++ ":" ++ show d))
                  return (n % d)
           <?> "split ratio as 'new-units : old-units'"

quantity :: Parser A.Quantity
quantity =   do q <- value
                when (q <= 0) (fail "number of units must be greater than zero")
                return q
         <?> "number of units (fractional units are permitted)"

cost :: A.Quantity -> Parser A.Cost
cost qty = do cst <-     (valueExt qty <?> "transaction cost (including commissions)")
                     <|> pricePerUnit qty
              when (cst < 0) (fail "cost cannot be negative")
              return cst

proceeds :: A.Quantity -> Parser A.Proceeds
proceeds qty = do pro <-     (valueExt qty <?> "transaction proceeds (net of commissions)")
                         <|> pricePerUnit qty
                  when (pro < 0) (fail "proceeds cannot be negative")
                  return pro

pricePerUnit :: A.Quantity -> Parser A.Value
pricePerUnit qty =   do symbol "@"
                        pr <- valueExt qty
                        return (qty * pr)
                 <?> "price per unit as '@ number'"

valueExt :: A.Quantity -> Parser Rational
valueExt qty =   lexeme value'
             <|> parens (exprExt qty)

exprExt :: A.Quantity -> Parser Rational
exprExt qty = buildExpressionParser table (factorExt qty) <?> "expression"

factorExt :: A.Quantity -> Parser Rational
factorExt qty =   parens (exprExt qty)
              <|> lexeme value'
              <|> do reserved "n"
                     return qty
              <?> "simple expression (use 'n' for number of units)"

value :: Parser Rational
value =   lexeme value'
      <|> parens expr

expr :: Parser Rational
expr = buildExpressionParser table factor <?> "expression"

factor :: Parser Rational
factor =   parens expr
       <|> lexeme value'
       <?> "simple expression"

table = [[op "*" (*) AssocLeft, op "/" (/) AssocLeft],
         [op "+" (+) AssocLeft, op "-" (-) AssocLeft]
        ]
    where op s f assoc =
              Infix (do { reservedOp s; return f } <?> "operator") assoc
        
value' :: Parser Rational
value' = do d <- decimal
            f <- option 0 fraction <?> "optional fractional part"
            return (toRational d + f)

fraction :: Parser Rational
fraction = do char '.'
              d <- decimal <?> "digits after decimal point"
              return (d % ceiling10 d)
                  where ceiling10 0 = 1
                        ceiling10 1 = 1
                        ceiling10 n = 10 * ceiling10 (n `div` 10)

-----------------------------------------

pt = parseTest (do journalParser; return ())
