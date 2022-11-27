{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Calculator where

import Parser
import Data.Fix(Fix(..))
import Data.Functor.Foldable (cata)
import Control.Applicative
import Text.Show.Deriving
import Data.Eq.Deriving
import Data.Functor

data AstF a b =
   BinaryF Operation b b |
   LitF a deriving (Show, Eq, Functor, Foldable)

data Operation = Plus | Multiply deriving (Eq, Show)

$(deriveShow1 ''AstF)
$(deriveEq1 ''AstF)

type Ast a = Fix(AstF a )

lit :: a -> Ast a
lit = Fix . LitF

binary :: Operation -> Ast a -> Ast a -> Ast a
binary o a1 a2 = Fix $ BinaryF o a1 a2

add :: Ast a -> Ast a -> Ast a
add = binary Plus

multiply :: Ast a -> Ast a -> Ast a
multiply = binary Multiply

-- remove in prod
instance Num a => Num (Ast a) where
  (+) = add
  (*) = multiply
  fromInteger = lit . fromInteger

algebra :: Num a => AstF a a -> a
algebra (LitF a) = a
algebra (BinaryF Plus a1 a2) = a1 + a2
algebra (BinaryF Multiply a1 a2) = a1 * a2

evaluate :: Num a => Ast a -> a
evaluate = cata algebra

operation :: Parser Operation
operation = surround spaces $ char '+' $> Plus <|> char '*' $> Multiply

parser :: Parser a -> Parser (Ast a)
parser p = litP <|> binaryP where
   litP = lit <$> p
   binaryP = parenthesis $ do
      a1 <- parser p
      o <- operation
      a2 <- parser p
      return $ binary o a1 a2

parse:: String -> Maybe (Ast Integer)
parse = run (parser int)
