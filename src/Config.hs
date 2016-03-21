module Config (
   getSettings,
   Config(..)
)where

import Data.Conf
import Control.Exception(try, SomeException)
import Control.Applicative((<*>), pure)

data Config = Config {
   ipsToValidate :: [String],
   mailServer :: String,
   useSsl :: Bool,
   ignoreCertFailure :: Maybe Bool,
   port :: Maybe Integer,
   username :: String,
   password :: String
} deriving Show

getSettings :: FilePath -> IO (Maybe Config)
getSettings configPath = do
   result <- try $ readConf configPath :: ReadConfType
   case result of
        Left _ -> return Nothing
        Right conf -> return $ buildConfig conf

type ReadConfType = IO (Either SomeException [(String, String)])

buildConfig conf =
   pure Config
   <+> "ipsToValidate"
   <+> "imapServer"
   <+> "imapUseSsl"
   <?> "imapIgnoreCertFailure"
   <?> "imapPort"
   <+> "imapUsername"
   <+> "imapPassword"
   where
      g <+> a = g <*> getConf a conf
      g <?> a = g >>= \c -> return $ c (getConf a conf)
