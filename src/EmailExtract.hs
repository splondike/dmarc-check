module EmailExtract(
   extractEmail,
   maybeUngzipBody,
   Email(..)
) where

import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Base64 as Base64

import Text.ParserCombinators.Parsec (parse)
import Data.String.Utils (startswith, split, join, strip, replace)
import System.Time (CalendarTime)
import qualified Text.ParserCombinators.Parsec.Rfc2822 as EParse
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Zip as Zip

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

   let xmlVal = extractReportXml message
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

extractReportXml (EParse.Message headers body) = content
   where
      content
         | isZipMimetype contentType = maybeUnzipBody maybeBodyStr
         | isGzipMimetype contentType = maybeUngzipBody maybeBodyStr
         | isXmlMimetype contentType = maybeBodyStr
         | startswith "multipart/mixed" contentType = extractMultipartEmail contentType body
         | otherwise = Nothing
      maybeBodyStr = eitherToMaybe .
                     (fmap LazyByteString.fromStrict) .
                     Base64.decode .
                     ByteString.pack .
                     (replace "\r\n" "") $ body
      contentType = case extractHeaders "Content-Type" headers of
                         [] -> "unknown"
                         (x:xs) -> x

extractMultipartEmail contentType body = result
   where
      result = case filter isJust $ map extractReportXml boundaryChunks of
                    (x:xs) -> x
                    [] -> Nothing
      boundaryChunks = case maybeBoundaryDelimiter of 
                            Just delimiter -> extractParts delimiter body
                            Nothing -> []
      maybeBoundaryDelimiter = lookup "boundary" $ parseMultiPartHeader contentType

maybeUnzipBody maybeByteStr = maybeXmlStr
   where
      maybeXmlStr = do
         archive <- maybeArchive
         let files = Zip.filesInArchive archive
         firstFile <- if 1 == (length files) then Just (head files) else Nothing
         entry <- Zip.findEntryByPath firstFile archive
         return $ Zip.fromEntry entry
      maybeArchive = maybeByteStr >>= (eitherToMaybe . Zip.toArchiveOrFail)

-- TODO: This library throws exceptions...
maybeUngzipBody maybeByteStr = fmap GZip.decompress maybeByteStr

extractHeaders name = foldl addIfMatches []
   where
      addIfMatches c (EParse.OptionalField fieldName fieldValue)
         | name == fieldName = (strip fieldValue):c
      addIfMatches c _ = c

isZipMimetype mime = startswith "application/zip" mime ||
                     startswith "application/x-zip-compressed" mime

isGzipMimetype mime = startswith "application/gzip" mime

isXmlMimetype mime = startswith "text/xml" mime

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
