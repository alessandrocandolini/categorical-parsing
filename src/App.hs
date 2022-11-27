module App where
import Options.Applicative
import Options (argsParserInfo, Args (expr))
import Calculator (parse, evaluate)

program :: IO ()
program = execParser argsParserInfo >>= print . fmap evaluate . parse . expr
