module Config (
   getSettings,
   Config(..)
)where

import Data.Conf
import Control.Exception(try, SomeException)

data Config = Config {
   ipsToValidate :: [String],
   mailServer :: String,
   username :: String,
   password :: String,
   reportRecipient :: String
} deriving Show

getSettings :: IO (Maybe Config)
getSettings = do
   result <- try $ readConf "dmarc-check.conf" :: ReadConfType
   case result of
        Left _ -> return Nothing
        Right conf -> return $ buildConfig conf

type ReadConfType = IO (Either SomeException [(String, String)])

buildConfig conf = do
   ipsToValidateVal <- getConf "ipsToValidate" conf
   mailServerVal <- getConf "mailServer" conf
   usernameVal <- getConf "username" conf
   passwordVal <- getConf "password" conf
   reportRecipientVal <- getConf "reportRecipient" conf
   return Config {
      ipsToValidate = ipsToValidateVal,
      mailServer = mailServerVal,
      username = usernameVal,
      password = passwordVal,
      reportRecipient = reportRecipientVal
   }
