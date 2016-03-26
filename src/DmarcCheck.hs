{-# LANGUAGE ScopedTypeVariables #-}
module DmarcCheck(
   program
) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (forM_, when)
import Data.Maybe (catMaybes)
import qualified Control.Exception as Ex

import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString.Lazy (toStrict)
import qualified Data.Csv as CSV

import Config
import EmailHandler
import EmailExtract
import qualified Data.DMARCAggregateReport as DM
import qualified CSVReport as CSV

program configPath includeCsvHeader markProcessedAsRead = do
   maybeConf <- getSettings configPath
   case maybeConf of
        Right conf -> processReports includeCsvHeader markProcessedAsRead conf
        Left msg -> usage msg

processReports includeHeader markProcessedAsRead conf = do
   conn <- try "connect to IMAP server" $ getReceiveConnection conf
   reportEmails <- try "download latest emails" $ getLatestReports conn
   csvRecords <- try "build CSV records" $ makeCsvRecords reportEmails

   putStr $ convertToCsv includeHeader csvRecords

   when markProcessedAsRead $ try "mark emails as read" $
        forM_ reportEmails $ \(id, _) ->
            markAsRead conn id
   where
      try :: forall a. String -> IO a -> IO a
      try errMsg action = do
         result <- Ex.try $ action :: IO (Either Ex.SomeException a)
         case result of
              Right val -> return val
              Left ex -> do
                 hPutStrLn stderr $ "Failed to " ++ errMsg ++ ":\n  " ++ (show ex)
                 exitFailure

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
