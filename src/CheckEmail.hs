module CheckEmail(
   emailToResult,
   Result(..)
) where

import Data.ByteString.Internal(ByteString)
import Data.ByteString.Char8 (unpack)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Types
import Text.XML.Light.Proc 
import EmailExtract(Email(..))
import Config (Config(..))

data Result = Pass | Fail | ParseError
   deriving (Show, Eq)

emailToResult :: Config -> Email -> Result
emailToResult conf email = toResult $ extractDOM >>= return . (getResult conf)
   where
      extractDOM = (xmlStr email) >>= parseXMLDoc
      toResult Nothing = ParseError
      toResult (Just r) = r

-- TODO: Check this works as expected with some constructed documents
getResult conf rootElm = if allOk then Pass else Fail
   where
      allOk = all checksPass $ filter ipMatches $ findElements (qn "row") rootElm

      checksPass elm = checkOk "dkim" elm && checkOk "spf" elm
      checkOk name elm = toBool $ find name elm >>= return . (=="pass") . strContent

      ipMatches elm = toBool $ find "source_ip" elm >>= return . checkIp
      checkIp elm = elem (strContent elm) validIps
      validIps = ipsToValidate conf

      find name elm = findElement (qn name) elm
      qn name = QName name Nothing Nothing
      toBool Nothing = False
      toBool (Just b) = b
