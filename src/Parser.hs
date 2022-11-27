{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where
import Control.Monad (mfilter, (>=>))
import Data.Functor
import Control.Applicative 
import Data.Char (isDigit, isSpace)
import Control.Monad.State
import Control.Natural

type Parser a = CoreParser Maybe String a 

newtype CoreParser m s a = Parser {
   unparse :: StateT s m a 
   } deriving (Functor, Applicative, Monad, Alternative, MonadPlus) 

{--
newtype Parser a = Parser {
  parse :: String -> Maybe (String, a)
} deriving (Functor)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap f . p

instance Applicative Parser where
  pure a = Parser $ \s -> pure (s, a)
  (Parser p1) <*> (Parser p2) = Parser p where
      p = p1 >=> (\ o -> (fmap (fmap (snd o)) . p2 . fst) o)

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser p where
     p s = p1 s <|> p2 s

run :: Parser a -> String -> Maybe a
run p = fmap snd . mfilter (null . fst) . parse p

anyChar :: Parser Char
anyChar = Parser p where
   p [] = Nothing
   p (c : cs) = Just (cs,c)
--}

run :: Parser a -> String -> Maybe a
run p = fmap fst . mfilter (null . snd) . runStateT (unparse p)

anyChar :: Parser Char
anyChar = Parser (StateT p) where
   p [] = empty
   p (c : cs) = pure (c,cs)

char :: Char -> Parser Char
char c = mfilter (== c) anyChar

digit :: Parser Char
digit = mfilter isDigit anyChar

string :: String -> Parser String
string = traverse char

bool :: Parser Bool
bool = true <|> false where
   true = string "True" $> True
   false = string "False" $> False

int :: Parser Int
int = fmap read (some digit)

ws :: Parser String
ws = some (mfilter isSpace anyChar) 
