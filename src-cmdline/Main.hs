import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified System.Console.GetOpt as GO

import qualified DmarcCheck as DmarcCheck

main = do
   opts <- getOpts
   DmarcCheck.program (configPath opts) (includeHeader opts)

getOpts = do
   rawArgs <- getArgs
   case GO.getOpt GO.Permute options rawArgs of
      (o,n,[]  ) -> return $ foldl (flip id) defaultOpts o
      (_,_,errs) -> ioError (userError (concat errs ++ usage))

data Opts = Opts {
   configPath :: String,
   includeHeader :: Bool
} deriving (Show)

defaultOpts = Opts {
   configPath = "dmarc-check.conf",
   includeHeader = True
}

options = [config, includeHeader]
   where
      config = GO.Option "c" ["config"] (GO.ReqArg (\val opts -> opts { configPath = val}) "CONFIG") "The configuration file containing IMAP connection information"
      includeHeader = GO.Option "" ["hideHeaders"] (GO.NoArg (\opts -> opts {includeHeader = False})) "Suppress the CSV headers"

usage = GO.usageInfo "Usage: dmarc-check --config <dmarc-check.conf> --hideHeaders\n\nProduce a CSV report based on the mailbox referenced in dmarc-check.conf" options
