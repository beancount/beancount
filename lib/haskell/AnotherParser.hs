module AnotherParser where

import List
import Data.Char
import System.IO

-- |A line is string of characters (end-of-line has been stripped) paired with
-- its line number.
type Line = (Int, String)

-- |A line group is either a list of a single line representing a directive or
-- a list of multiple lines representing an entry; in the latter case, the
-- head of the group is the entry declaration and the tail is the list of
-- transactions for the entry.
type LineGroup = [Line]

-- Put your favorite file name here
fileName = "fxt.ledger"

main = do input <- readFile fileName
          let items = journal input
          mapM_ (putStrLn . show) items
          putStrLn ("Found " ++ show (length items) ++ " items")

journal :: String -> [LineGroup]
journal = groupLines . mkLines

-- |Break down the input into a list of lines, paired them up with line
-- numbers, and drop all comment and empty lines.
mkLines :: String -> [Line]
mkLines s = filter (not . isComment . snd) (zip [1..](lines s))

-- |A comment is a line that is empty or starts with ';' or only contains
-- white space.
isComment :: String -> Bool
isComment []     = True
isComment (c:cs) | c == ';'  = True
                 | otherwise = and . map isSpace $ (c:cs)

-- |Lines are grouped so that all transactions lines for an entry are in the same
-- line group as the entry line.
-- We accomplish this by passing around an extra parameter used to accumulate
-- all the lines for the current group.
groupLines :: LineGroup -> [LineGroup]
groupLines = tail . groupLines' []
    where
      groupLines' cg [] = [cg]
      groupLines' cg (l:ls) | isTrans . snd $ l = groupLines' (cg ++ [l]) ls
                            | otherwise         = cg : groupLines' [l] ls

-- |A transaction is a line that starts with space.
-- This function should not be called on lines that consists only of white space.
isTrans :: String -> Bool
isTrans (c:cs) = isSpace c


