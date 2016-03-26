module EmailHandler (
   getReceiveConnection,
   getLatestReports,
   markAsRead,
   ReceiveConnection,
   MessageID
) where

import Control.Exception (try, SomeException)
import Data.ByteString.Internal(ByteString)
import qualified Data.Text.Lazy as Text (pack)

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Connection
import qualified Network.HaskellNet.IMAP as IPlain
import qualified Network.HaskellNet.IMAP.SSL as ISSL

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
   let getPairs id = fetch connection id >>= \content ->
                     -- Some servers mark messages as seen after fetch
                     -- we will mark them as read explicitly after
                     -- processing
                     store connection id (MinusFlags [Seen]) >>
                     return (MessageID id, content)
   mapM getPairs msgs

markAsRead :: ReceiveConnection -> MessageID -> IO ()
markAsRead receiveConnection (MessageID id) = do
   let (ReceiveConnection connection) = receiveConnection
   store connection id (PlusFlags [Seen])

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
