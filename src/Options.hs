{-# LANGUAGE DeriveGeneric #-}
module Options where
import Options.Generic
import Options.Applicative


newtype Args = Args {
   expr :: String
   } deriving (Eq,Show, Generic)

argsParser :: Parser Args
argsParser = Args <$> strOption
          ( long "expr"  <> short 'e' <> help "insert expression")

argsParserInfo :: ParserInfo Args
argsParserInfo = info (argsParser <**> helper)
      ( fullDesc <> progDesc "Run interpreter")
