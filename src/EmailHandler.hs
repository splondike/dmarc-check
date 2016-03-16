module EmailHandler (
   getReceiveConnection,
   getLatestReports,
   ReceiveConnection,
   MessageID
) where

import Data.ByteString.Internal(ByteString)
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Connection
import qualified Network.HaskellNet.IMAP as IPlain
import qualified Network.HaskellNet.IMAP.SSL as ISSL

import qualified Data.Text.Lazy as Text (pack)

import Config

data ReceiveConnection = ReceiveConnection IMAPConnection
data MessageID = MessageID UID

getReceiveConnection :: Config -> IO ReceiveConnection
getReceiveConnection conf = do
   let usernameVal = username conf
   let passwordVal = password conf
   connection <- connectToImap conf
   login connection usernameVal passwordVal
   select connection "INBOX"
   return (ReceiveConnection connection)

getLatestReports :: ReceiveConnection -> IO [(MessageID, ByteString)]
getLatestReports receiveConnection = do
   let (ReceiveConnection connection) = receiveConnection
   msgs <- search connection [UNFLAG Seen]
   let getPairs id = fetch connection id >>= (\c -> return (MessageID id, c))
   idsWithContent <- mapM getPairs msgs
   return idsWithContent

connectToImap conf = case useSsl conf of
                        True -> sslConnection
                        False -> plainConnection
   where
      sslConnection = ISSL.connectIMAPSSLWithSettings mailServerVal sslSettings
      plainConnection = IPlain.connectIMAPPort mailServerVal serverPort
      mailServerVal = mailServer conf
      sslSettings = ISSL.Settings {
         ISSL.sslPort = serverPort,
         ISSL.sslMaxLineLength = 1000000,
         ISSL.sslLogToConsole = False,
         ISSL.sslDisableCertificateValidation = disableCert
      }
      serverPort = fromInteger $ guessPort (useSsl conf) (port conf)
      guessPort _ (Just portOverride) = portOverride
      guessPort True _ = 993
      guessPort False _ = 143
      disableCert = handleMaybe $ ignoreCertFailure conf
      handleMaybe (Just b) = b
      handleMaybe Nothing = False
