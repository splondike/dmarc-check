module Data.DMARCAggregateReport.Parser (
   parseReport,
   ExtractFailure(..)
) where

import qualified Data.ByteString.Lazy as LazyByteString (fromStrict, ByteString)

import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Types (QName(..))
import qualified Text.XML.Light.Proc as P
import qualified System.Time as Time

import qualified Data.DMARCAggregateReport.Types as T

data ExtractFailure = XMLParseFailure | InvalidDocument
   deriving (Show, Eq)

-- | Converts a serialized XML report to a Haskell data type
parseReport :: LazyByteString.ByteString
           -- ^ A DMARC aggregate report serialized as XML
           -> Either ExtractFailure T.Report
parseReport xmlStr = case parseXMLDoc xmlStr of
                          Just dom -> processDOM dom
                          Nothing -> Left XMLParseFailure

processDOM rootElm = case processDOM' rootElm of
                  Just report -> Right report
                  Nothing -> Left InvalidDocument

processDOM' rootElm = pure T.Report
                        <*> orgName
                        <*> reportId
                        <*> beginDate
                        <*> endDate
                        <*> records
   where
      orgName = metadata >>= findContent "org_name"
      reportId = metadata >>= findContent "report_id"
      beginDate = dateElm >>= findContent "begin" >>= parseTimestamp
      endDate = dateElm >>= findContent "end" >>= parseTimestamp
      records = mapM parseRecord $ P.findElements (qn "record") rootElm

      dateElm = P.findElement (qn "date_range") rootElm
      metadata = P.findElement (qn "report_metadata") rootElm

      parseTimestamp str = safeRead str >>= (\v -> return $ Time.TOD v 0)

parseRecord recordElm = pure T.Record
                          <*> sourceIp
                          <*> headerFrom
                          <*> messageCount
                          <*> policyDkim
                          <*> policySpf
   where
      sourceIp = row >>= findContent "source_ip" >>= safeRead
      headerFrom =  identifiers >>= findContent "header_from"
      messageCount = row >>= findContent "count" >>= safeRead
      policyDkim = policy >>= findContent "dkim" >>= safeRead
      policySpf = policy >>= findContent "spf" >>= safeRead

      identifiers = P.findElement (qn "identifiers") recordElm
      row = P.findElement (qn "row") recordElm
      policy = row >>= P.findElement (qn "policy_evaluated")

findContent tagName elm = P.findElement (qn tagName) elm >>= return . P.strContent
qn tag = QName tag Nothing Nothing

safeRead :: (Read a) => String -> Maybe a
safeRead str = case reads str of
                    [(val, "")] -> Just val
                    _ -> Nothing
