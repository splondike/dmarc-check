module EmailHandler (
   getReceiveConnection,
   getLatestReports,
   sendMessage,
   ReceiveConnection,
   MessageID
) where

import Data.ByteString.Internal(ByteString)
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL (Settings(..), Flag(Seen), FlagsQuery(..), connectIMAPSSLWithSettings)

import qualified Data.Text.Lazy as Text (pack)
import qualified Network.HaskellNet.SMTP as SMTP (authenticate, sendPlainTextMail,  closeSMTP, SMTPConnection, AuthType(..))
import qualified Network.HaskellNet.SMTP.SSL as SMTPSSL (connectSMTPSSLWithSettings, Settings(..))

import Config

imapSettings = Settings {
   sslPort = 993,
   sslMaxLineLength = 1000000,
   sslLogToConsole = False,
   sslDisableCertificateValidation = True
}

smtpSettings = SMTPSSL.Settings {
   sslPort = 465,
   sslMaxLineLength = 1000000,
   sslLogToConsole = False,
   sslDisableCertificateValidation = True
}

data ReceiveConnection = ReceiveConnection IMAPConnection
data MessageID = MessageID UID

getReceiveConnection :: Config -> IO ReceiveConnection
getReceiveConnection conf = do
   let mailServerVal = mailServer conf
   let usernameVal = username conf
   let passwordVal = password conf
   connection <- connectIMAPSSLWithSettings mailServerVal imapSettings
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

getSendConnection :: Config -> IO SMTP.SMTPConnection
getSendConnection conf = do
   let mailServerVal = mailServer conf
   let usernameVal = username conf
   let passwordVal = password conf
   connection <- SMTPSSL.connectSMTPSSLWithSettings mailServerVal smtpSettings
   SMTP.authenticate PLAIN usernameVal passwordVal connection
   return connection

sendMessage :: Config -> String -> String -> String -> IO ()
sendMessage conf to subject msg = do
   let from = username conf
   let msgText = Text.pack msg
   connection <- getSendConnection conf
   SMTP.sendPlainTextMail to from subject msgText connection
   SMTP.closeSMTP connection
