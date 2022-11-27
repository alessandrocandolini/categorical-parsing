module Calculator where

import Parser 

data Ast a = 
   Add (Ast a) (Ast a) | 
   Multiply (Ast a) (Ast a) | 
   Lit a 

evaluate :: Num a => Ast a  -> a 
evaluate (Add v1 v2) = evaluate v1 + evaluate v2 
evaluate (Multiply v1 v2) = evaluate v1 * evaluate v2 
evaluate (Lit a) = a 

ast :: Parser a -> Parser (Ast a) 
ast = undefined -- add <|> multiply <|> lit

