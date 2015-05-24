{-# LANGUAGE OverloadedStrings #-}

module Language.Elisp.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text.Lazy hiding (D, I, Number)
import qualified Data.Attoparsec.Text.Lazy as A
import           Data.Monoid
import qualified Data.Text                 as T


data SExp = I Int
          | D Double
          | T T.Text
          | S T.Text
          | L [SExp]
          | F Fun [SExp]
          deriving (Eq)

instance Show SExp where
   show (L xs)   = '(' : unwords (map show xs) ++ ")"
   show (F f xs) = '(' : unwords (show f : map show xs) ++ ")"
   show (S s)    = T.unpack s
   show (T t)    = show t
   show (D d)    = show d
   show (I i)    = show i

nil :: SExp
nil = S "nil"

data Fun = Pure | Impure | Unknown T.Text deriving (Eq, Show)

parseElisp :: T.Text -> SExp
parseElisp = const (I 4)

parser :: Parser SExp
parser = (decimal ~> I)
     <|> (double  ~> D)
     <|> (text    ~> T)
     <|> (symbol  ~> S)
     <|> (list    ~> L)
     <|> return nil
  where x ~> y = y <$> (x <* delimiter)

list :: Parser [SExp]
list = do
       char '('
       xs <- many parser
       char ')'
       return xs

symbol :: Parser T.Text
symbol = do
         b <- A.option "" (string "'")
         a <- A.takeWhile1 (`notElem` delimiter')
         return $ b <> a

text :: Parser T.Text
text = do char '"'
          a <- loop ""
          char '"'
          return a
  where loop acc = do
           a <- A.takeWhile (A.notInClass "\\\"")
           p <- peekChar'
           if p == '"'
              then return $ acc <> a
              else do
                   x <- count 2 anyChar
                   join $ return (loop $ acc <> a <> T.pack x)

delimiter' :: String
delimiter' = "\"'\n\t ()"

delimiter :: Parser String
delimiter = do
            p <- peekChar'
            guard (p `elem` delimiter')
            many space
