import Data.Monoid ((<>))
import Control.Applicative (pure, (<*>))

import Options.Applicative

import qualified DmarcCheck as DmarcCheck

main = execParser progOpts >>= \opts ->
   DmarcCheck.program (configPath opts) (not $ hideHeader opts)

data Opts = Opts {
   configPath :: String,
   hideHeader :: Bool
} deriving (Show)

progOpts = info (helper <*> opts) fullDesc
   where
      opts = pure Opts 
         <*> strOption (long "config"
                     <> short 'c'
                     <> metavar "CONFIG"
                     <> help "The configuration file containing IMAP connection information")
         <*> switch    (long "hideHeader"
                     <> help "Suppress the CSV header line")
