{-# LANGUAGE QuasiQuotes #-}
module AstQuasiQuoter(ast) where


import Calculator
import Parser
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

qExp :: String -> TH.ExpQ
qExp = undefined

qPat :: String -> TH.PatQ
qPat = undefined

qType :: String -> TH.TypeQ
qType = undefined

ast  :: QuasiQuoter
ast  =  QuasiQuoter qExp qPat qType undefined
