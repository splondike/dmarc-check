module DmarcCheck(
   program
) where

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)

import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString.Lazy (toStrict)
import qualified Data.Csv as CSV

import Config
import EmailHandler
import EmailExtract
import qualified Data.DMARCAggregateReport as DM
import qualified CSVReport as CSV

program configPath includeHeader = do
   maybeConf <- getSettings configPath
   case maybeConf of
        Right conf -> processReports includeHeader conf
        Left msg -> usage msg

processReports includeHeader conf = do
   conn <- getReceiveConnection conf
   reportEmails <- getLatestReports conn
   csvRecords <- makeCsvRecords reportEmails
   putStrLn $ convertToCsv includeHeader csvRecords
   forM_ reportEmails $ \(id, _) ->
      markAsRead conn id

convertToCsv _ [] = ""
convertToCsv includeHeader csvRecords@(x:xs) = case decodeUtf8' bsResult of
                                      Right val -> unpack val
                                      Left _ -> ""
   where
      bsResult = toStrict $ CSV.encodeByNameWith encodeOptions header csvRecords
      header = CSV.headerOrder x
      encodeOptions = CSV.defaultEncodeOptions {
         CSV.encIncludeHeader = includeHeader
      }

makeCsvRecords emails = CSV.buildReports $ makeReports emails

makeReports emails = reports
   where
      reports = catMaybes $ map extractReport emails
      extractReport (id, content) = extractEmail content >>=
                                    xmlStr >>=
                                    eitherToMaybe . DM.parseReport
      eitherToMaybe (Left _) = Nothing
      eitherToMaybe (Right x) = Just x
      
usage msg = putStrLn msg >> exitFailure
