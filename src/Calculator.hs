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

instance Num a => Num (Ast a) where
  a1 + a2 = Fix $AddF a1 a2
  a1 * a2 = Fix $ MultiplyF a1 a2
  fromInteger = Fix . LitF . fromInteger

evalAlg :: Num a => AstF a a -> a
evalAlg (LitF a) = a
evalAlg (AddF a1 a2) = a1 + a2
evalAlg (MultiplyF a1 a2) = a1 * a2

evaluate :: Num a => Ast a -> a
evaluate = cata evalAlg

--evaluate :: Num a => Ast a  -> a
--evaluate (Add v1 v2) = evaluate v1 + evaluate v2
--evaluate (Multiply v1 v2) = evaluate v1 * evaluate v2
--evaluate (Lit a) = a

ast :: Parser a -> Parser (Ast a)
ast = undefined -- add <|> multiply <|> lit

