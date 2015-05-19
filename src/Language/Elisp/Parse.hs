module Language.Elisp.Parse where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text.Lazy hiding (D, I, Number)
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy            as T

data SExp = I Int
          | D Double
          | T T.Text
          | F Fun
          | List [SExp]
          deriving (Eq, Show)

data Fun = Pure | Impure | Unknown T.Text deriving (Eq, Show)

parseElisp :: T.Text -> SExp
parseElisp = const (I 4)

parser :: Parser SExp
parser = decimal ~> I
     <|> double  ~> D
     <|> text    ~> T
  where x ~> y = y <$> (delimiter *> x)

text = undefined

delimiter :: Parser String
delimiter = do
            p <- peekChar'
            guard (p `elem` "\n\t ()")
            many space
