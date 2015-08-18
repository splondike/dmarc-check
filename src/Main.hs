import System.Exit (exitFailure)
import Control.Monad (forM_)

import Config
import EmailHandler
import EmailExtract
import CheckEmail

main = do
   maybeConf <- getSettings
   case maybeConf of
        Just conf -> processReports conf
        Nothing -> usage

processReports conf = do
   receiveConn <- getReceiveConnection conf
   reportEmailStrs <- getLatestReports receiveConn
   let maybeReportEmails = map (extractEmail . snd) reportEmailStrs
   let onlyParsed = concatMap maybeToList maybeReportEmails
   let results = map (emailToResult conf) onlyParsed

   let processingFailures = (length maybeReportEmails) - (length onlyParsed)
   let resultPairs = zip results onlyParsed
   sendEmail conf resultPairs processingFailures
   where
      maybeToList (Just a) = [a]
      maybeToList Nothing = []

sendEmail :: Config -> [(Result, Email)] -> Int -> IO ()
sendEmail conf resultPairs parseFailuresCount = sendMessage conf to subj msg
   where
      to = reportRecipient conf
      msg = resultStr resultPairs parseFailuresCount
      passed = all ((==Pass) . fst) resultPairs
      subj = if passed then "Dmarc: passed" else "Dmarc: failed"

resultStr resultPairs failuresCount = concat msg
   where
      msg = ["Received ", totalEmails, " emails.", parseFailuresStr, "\n\n", failedChecksStr]
      totalEmails = show $ failuresCount + (length resultPairs)
      parseFailuresStr = if failuresCount > 0 then parseFailuresStr' else ""
      parseFailuresStr' = " Failed to parse " ++ (show failuresCount) ++ " emails."
      failedChecksStr = if length failedEmails > 0 then "Failed emails:\n" ++ failedEmailsStr else ""
      failedEmailsStr = concatMap emailToStr failedEmails
      emailToStr e = concat [subject e, " (", show $ dateReceived e, ")\n"]
      failedEmails = concatMap extractFailedEmail resultPairs
      extractFailedEmail (Pass, _) = []
      extractFailedEmail (_, email) = [email]

usage = putStrLn "Check you have a dmarc-check.conf file fully filled out in the current working directory." >> exitFailure
