module EmailExtract(
   extractEmail,
   Email(..)
) where

import Data.Char (toLower)
import qualified Data.ByteString.Char8 as ByteString (pack, unpack, ByteString)
import qualified Data.ByteString.Lazy as LazyByteString (fromStrict, ByteString)
import qualified Data.ByteString.Base64 as Base64 (decode)
import Data.String.Utils
import Text.ParserCombinators.Parsec (parse)
import qualified Text.ParserCombinators.Parsec.Rfc2822 as EParse
import Codec.Archive.Zip
import System.Time (CalendarTime)
import Data.List(find)
import Data.Maybe(isJust)

data Email = Email {
   subject :: String,
   dateReceived :: CalendarTime,
   xmlStr :: Maybe LazyByteString.ByteString
}

extractEmail :: ByteString.ByteString -> Maybe Email
extractEmail msgStr = do
   message <- eitherToMaybe $ parse EParse.message "" $ ByteString.unpack msgStr
   let (EParse.Message headers _) = message
   subjectStr <- extractHeader getSubject headers
   dateReceivedCal <- extractHeader getReceived headers

   let xmlVal = maybeZippedContent message >>= unzipBody . (replace "\r\n" "")
   return Email {
      subject = subjectStr,
      dateReceived = dateReceivedCal,
      xmlStr = xmlVal
   }
   where
      extractHeader extractor headers = find (isJust . extractor) headers >>= extractor
      getSubject (EParse.Subject v) = Just v
      getSubject _ = Nothing
      getReceived (EParse.Received (_, cal)) = Just cal
      getReceived _ = Nothing

unzipBody base64ZipStr = maybeXmlStr
   where
      maybeXmlStr = do
         archive <- maybeArchive
         let files = filesInArchive archive
         firstFile <- if 1 == (length files) then Just (head files) else Nothing
         entry <- findEntryByPath firstFile archive
         return $ fromEntry entry
      maybeArchive = maybeArchiveByteStr >>= (eitherToMaybe . toArchiveOrFail)
      maybeArchiveByteStr = decodedByteStr >>= return . LazyByteString.fromStrict
      decodedByteStr = eitherToMaybe . Base64.decode $ ByteString.pack base64ZipStr

maybeZippedContent (EParse.Message headers body) = content
   where
      content
         | isZipMimetype contentType = Just body
         | startswith "multipart/mixed" contentType = extractMultipartZip contentType body
         | otherwise = Nothing
      contentType = case extractHeaders "Content-Type" headers of
                         [] -> "unknown"
                         (x:xs) -> x

extractHeaders name = foldl addIfMatches []
   where
      addIfMatches c (EParse.OptionalField fieldName fieldValue)
         | name == fieldName = (strip fieldValue):c
      addIfMatches c _ = c

extractMultipartZip :: String -> String -> Maybe String
extractMultipartZip contentType body = zipBody
   where
      zipBody = zipMessageChunk >>= (\(EParse.Message _ b) -> return b)
      zipMessageChunk = case filter isZipChunk boundaryChunks of
                      [] -> Nothing
                      (x:xs) -> Just x
      isZipChunk (EParse.Message headers _) = let allHeaders = extractHeaders "Content-Type" headers
                                       in any isZipMimetype allHeaders
      boundaryChunks = case maybeBoundaryDelimiter of 
                            Just delimiter -> extractParts delimiter body
                            Nothing -> []
      maybeBoundaryDelimiter = lookup "boundary" $ parseMultiPartHeader contentType

isZipMimetype mime = startswith "application/zip" mime || startswith "application/x-zip-compressed" mime

extractParts :: String -> String -> [EParse.Message]
extractParts delimiter body = justSuccessful
   where
      justSuccessful = foldl removeFailed [] allParsed
      removeFailed c (Left _) = c
      removeFailed c (Right m) = c ++ [m]
      allParsed = map (parse EParse.message "") allChunks
      (_, allChunks) = foldl handleLine ([], []) (split "\r\n" body)
      handleLine (buffer, chunks) line
         | startswith fullDelimiter line = ([], chunks ++ [(join "\r\n" buffer)])
         | otherwise = (buffer ++ [line], chunks)
      fullDelimiter = "--" ++ delimiter

-- | Turns a 'asrt; blah=foo; bar="foobar"' style header into a
-- list of key/value pairs
parseMultiPartHeader :: String -> [(String, String)]
parseMultiPartHeader val = map parseBit $ split ";" val
   where
      parseBit chunk = let cleanChunk = strip chunk
                       in (key cleanChunk, value cleanChunk)
      key cleanChunk = head $ split "=" cleanChunk
      value cleanChunk = replace "\"" "" $ tail $ dropWhile (/= '=') cleanChunk

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val
