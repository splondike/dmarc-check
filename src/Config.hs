module Config (
   getSettings,
   Config(..)
)where

import Data.Conf
import Control.Exception(try, SomeException)
import Control.Applicative((<*>), pure)

data Config = Config {
   mailServer :: String,
   useSsl :: Bool,
   ignoreCertFailure :: Maybe Bool,
   port :: Maybe Integer,
   username :: String,
   password :: String
} deriving Show

getSettings :: FilePath -> IO (Either String Config)
getSettings configPath = do
   result <- try $ readConf configPath :: ReadConfType
   case result of
        Left _ -> return $ Left $ "Couldn't read config file at " ++ configPath
        Right conf -> return $ buildConfig conf

type ReadConfType = IO (Either SomeException [(String, String)])

buildConfig :: [(String, String)] -> Either String Config
buildConfig conf =
   pure Config
   <+> "imapServer"
   <+> "imapUseSsl"
   <?> "imapIgnoreCertFailure"
   <?> "imapPort"
   <+> "imapUsername"
   <+> "imapPassword"
   where
      g <+> a = g <*> getConfEither a conf
      g <?> a = g >>= \c -> return $ c (getConf a conf)
      getConfEither key map = case getConf key map of
                                   Just val -> Right val
                                   Nothing -> Left $ "Couldn't find expected field " ++ key
