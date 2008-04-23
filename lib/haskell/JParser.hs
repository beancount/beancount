module Main where

import Text.ParserCombinators.Parsec    
import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Char (isSpace, isDigit)

-- Put your favorite file name here
fname = "blais.ledger"

main = do st <- startState
          input <- readFile fname
          let res = runParser journal st fname input
          case res of
            Left err -> do putStr "parse error at " ; print err
            Right rs -> mapM_ (putStrLn . show) rs
            -- I use mapM_ to print one record per line; otherwise, calling
            -- show of a list will put everything on the same line.

type Directive = String

data Entry = Ent { entDate :: Date
                 , entEffectiveDate :: Date
                 , entStatus :: Status
                 , entCode :: Code
                 , entPayee :: Payee
                 , entNote :: Note
                 , entTransactions :: [Transaction]
                 } deriving Show

type Date = Day

data Status = Unbalanced | Pending | Cleared
            deriving Show

type Code = String

data Transaction = Tran { tranStatus :: Status
                        , tranAccount :: Account
                        , tranAmount :: Maybe Amount
                        , tranNote :: Note
                        } deriving Show

type Payee = String

type Account = [String]

-- The second Amount' is used as a price per unit. If a price per unit is not specified,
-- it will be set to one unit of the original commodity (the one given in the first Amount').
type Amount = (Amount', Amount')

type Amount' = (Quantity, Commodity)

type Quantity = Float
type Commodity = String
type Note = String


newtype JrnlState = JS { year :: Integer }
type JrnlParser = GenParser Char JrnlState

startState :: IO JrnlState
startState = return JS { year = 2008 }

-- This is used to build lexeme parsers, i.e. parsers that consume trailing white space so
-- that the next non-white character is ready for the next parser to consume.
lexeme :: JrnlParser a -> JrnlParser a
lexeme p = do res <- p
              many (oneOf " \t")
              return res

journal :: JrnlParser [Entry]
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
emptyLine :: JrnlParser (Maybe Entry)
emptyLine = do many (oneOf " \t")
               newline
               return Nothing
          <?> "empty line"

comment :: JrnlParser (Maybe Entry)
comment = do oneOf ";*hb"
             skipMany (satisfy (/= '\n'))
             newline
             return Nothing
        <?> "comment"

directive :: JrnlParser (Maybe Entry)
directive = do satisfy (\c -> not (';' == c || isSpace c || isDigit c))
               newline
               return Nothing
          <?> "directive"

entry :: JrnlParser (Maybe Entry)
entry = do e <- plainEntry
           return (Just e)

plainEntry :: JrnlParser Entry
plainEntry = do (d, ed) <- lexeme fullDate
                st <- option Unbalanced (lexeme status)
                cd <- option "" (lexeme code)
                p <- payee
                n <- option "" note
                newline
                ts <- many1 transaction
                return (Ent { entDate = d,
                              entEffectiveDate = ed,
                              entStatus = st,
                              entCode = cd,
                              entPayee = p,
                              entNote = n,
                              entTransactions = ts } )

fullDate :: JrnlParser (Date, Date)
fullDate = do d <- date <?> "date"
              ed <- option d (char '=' >> date) <?> "effective date"
              return (d, ed)

date :: JrnlParser Date
date = do sy <- count 4 digit
          dateSep
          sm <- count 2 digit
          dateSep
          sd <- count 2 digit
          let y = read sy
              m = read sm
              d = read sd
          if m > 12 || d > (julianMonthLength y m) then
              fail "invalid date"
           else
              return (fromGregorian y m d)

dateSep :: JrnlParser ()
dateSep = do oneOf "/-."
             return ()

status :: JrnlParser Status
status = (char '!' >> return Pending) <|> (char '*' >> return Cleared) <?> "status"

code :: JrnlParser Code
code = between (char '(') (char ')') (many (noneOf ")\n")) <?> "code"

payee :: JrnlParser Payee
payee = many (satisfy (/= '\n')) <?> "payee"

transaction :: JrnlParser Transaction
transaction = do many1 (oneOf " \t")
                 st <- option Unbalanced (lexeme status)
                 acc <- lexeme account
                 amt <- optionMaybe (lexeme amount)
                 n <- option "" note
                 newline
                 return (Tran { tranStatus = st,
                                tranAccount = acc,
                                tranAmount = amt,
                                tranNote = n } )

account :: JrnlParser Account
account = accountNameList
      <|> between (char '(') (char ')') accountNameList
      <|> between (char '[') (char ']') accountNameList
      <?> "account"

accountNameList :: JrnlParser Account
accountNameList = do sepBy1 accountName (char ':')

-- The 'try' combinator provides backtracking when its parser fails. Here, the failure will
-- stop the mutual recursion and result in an account name free of trailing white space.
accountName :: JrnlParser String
accountName = do w <- many1 (noneOf ":)] \t;\n")
                 ws <- (try accountName') <|> return ""
                 return (w ++ ws)
                 return w

-- An account name may contain white space, except if the white space is followed by the
-- beginning of the <amount>, <note>, or newline (whatever can legally follow the account
-- production). The notFollowedBy combinator makes it so easy! It fails if its parser
-- succeeds; in this case, the entire accountName' parser fails, giving the try combinator
-- above a chance to backtrack; if notFollowedBy succeeds, parsing of the account name
-- continues on with mutual recursion.
accountName' :: JrnlParser String
accountName' = do s1 <- many1 (oneOf " \t")
                  notFollowedBy (digit <|> oneOf "-;\n")
                  s2 <- accountName
                  return (s1 ++ s2)

amount :: JrnlParser Amount
amount = do (q, c) <- amount'
            pr <- option (1, c) (lexeme (char '@') >> amount') -- price per unit
            return ((q, c), pr)

amount' :: JrnlParser Amount'
amount' = do q <- lexeme quantity
             c <- lexeme commodity
             return (q, c)

quantity :: JrnlParser Quantity
quantity = do s <- option ' ' (char '-')
              d <- many1 digit
              f <- option "0" (char '.' >> many1 digit)
              return (read (s:d ++ "." ++ f))
         <?> "quantity"

commodity :: JrnlParser Commodity
commodity = do c <- letter
               cs <- many alphaNum
               return (c:cs)
          <?> "commodity"

note :: JrnlParser String
note = do char ';'
          cs <- many (satisfy (/= '\n'))
          return cs
     <?> "note"
