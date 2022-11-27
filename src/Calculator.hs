{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Calculator where

import Parser
import Data.Fix(Fix(..))
import Data.Functor.Foldable (cata)
import Control.Applicative
import Data.Functor.Classes
import Text.Show.Deriving
import Data.Eq.Deriving

data AstF a b =
   AddF b b |
   MultiplyF  b b |
   LitF a deriving (Show, Eq, Functor, Foldable)

$(deriveShow1 ''AstF)
$(deriveEq1 ''AstF)

type Ast a = Fix(AstF a )


lit :: a -> Ast a
lit = Fix . LitF

add :: Ast a -> Ast a -> Ast a
add a1 a2 = Fix $ AddF a1 a2

multiply :: Ast a -> Ast a -> Ast a
multiply a1 a2 = Fix $ MultiplyF a1 a2

-- remove in prod
instance Num a => Num (Ast a) where
  (+) = add
  (*) = multiply
  fromInteger = Fix . LitF . fromInteger

algebra :: Num a => AstF a a -> a
algebra (LitF a) = a
algebra (AddF a1 a2) = a1 + a2
algebra (MultiplyF a1 a2) = a1 * a2

evaluate :: Num a => Ast a -> a
evaluate = cata algebra

parser :: Parser a -> Parser (Ast a)
parser p = litP <|> addP <|> multiplyP where
   litP = lit <$> p
   addP = parenthesis $ do
      a1 <- parser p
      _ <- char '+'
      a2 <- parser p
      return $ add a1 a2
   multiplyP = parenthesis $ do
      a1 <- parser p
      _ <- char '*'
      a2 <- parser p
      return $ multiply a1 a2



parse:: String -> Maybe (Ast Integer)
parse = run (parser int)
