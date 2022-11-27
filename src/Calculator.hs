{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
module Calculator(Ast, evaluate) where

import Parser
import Data.Fix(Fix(..))
import Data.Functor.Foldable (cata)

data AstF a b =
   AddF b b |
   MultiplyF  b b |
   LitF a deriving (Functor, Foldable)

type Ast a = Fix(AstF a )

-- remove in prod
instance Num a => Num (Ast a) where
  a1 + a2 = Fix $AddF a1 a2
  a1 * a2 = Fix $ MultiplyF a1 a2
  fromInteger = Fix . LitF . fromInteger

algebra :: Num a => AstF a a -> a
algebra (LitF a) = a
algebra (AddF a1 a2) = a1 + a2
algebra (MultiplyF a1 a2) = a1 * a2

evaluate :: Num a => Ast a -> a
evaluate = cata algebra

ast :: Parser a -> Parser (Ast a)
ast = undefined -- add <|> multiply <|> lit

