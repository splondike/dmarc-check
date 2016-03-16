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
   username :: String,
   password :: String
} deriving Show

getSettings :: IO (Maybe Config)
getSettings = do
   result <- try $ readConf "dmarc-check.conf" :: ReadConfType
   case result of
        Left _ -> return Nothing
        Right conf -> return $ buildConfig conf

type ReadConfType = IO (Either SomeException [(String, String)])

buildConfig conf =
   pure Config
   <+> "ipsToValidate"
   <+> "mailServer"
   <+> "username"
   <+> "password"
   where
      g <+> a = g <*> getConf a conf
