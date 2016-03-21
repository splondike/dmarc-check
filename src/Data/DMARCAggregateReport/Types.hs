{-|
Description : Haskell data types for the DMARC aggregate report format: https://tools.ietf.org/html/rfc7489#appendix-C
-}
module Data.DMARCAggregateReport.Types (
   Report(..),
   Record(..),
   DKIMResult(..),
   SPFResult(..)
) where

import Data.Maybe (catMaybes)

import qualified Data.IP as IP
import qualified System.Time as Time

data Report = Report {
   -- | The name of the organiztion who generated the report
   orgName :: String,
   -- | The unique identifier for this report within orgName
   reportId :: String,
   -- | The start of the time range covered by the report
   beginDate :: Time.ClockTime,
   -- | The end of the time range covered by the report
   endDate :: Time.ClockTime,
   -- | The validation results. Each row corresponds to an
   -- (sender IP address, sender domain, validation result) tuple.
   records :: [Record]
} deriving (Show, Eq)

data Record = Record {
   -- | The IP that sent the emails
   sourceIp :: IP.IP,
   -- | The email From header was from this domain
   headerFrom :: String,
   -- | The number of emails sent by the above IP to the receiver in the report time range
   messageCount :: Integer,
   -- | The final result of verifying the emails against the domain's DKIM policy
   policyDkim :: DKIMResult,
   -- | The final result of verifying the emails against the domain's SPF policy
   policySpf :: SPFResult
} deriving (Show, Eq)

-- | See https://tools.ietf.org/html/rfc7001#section-2.6.1 for what these mean.
-- If you have a DKIM policy on your domain, only DKIMPass should be considered a success.
data DKIMResult = DKIMPass | DKIMNone | DKIMFail | DKIMPolicy | DKIMNeutral | DKIMTempError | DKIMPermError
   deriving (Eq)

instance Read DKIMResult where
   readsPrec _ str = readsResult [
      ("pass", DKIMPass),
      ("none", DKIMNone),
      ("fail", DKIMFail),
      ("policy", DKIMPolicy),
      ("neutral", DKIMNeutral),
      ("temperror", DKIMTempError),
      ("permerror", DKIMPermError)] str


instance Show DKIMResult where
   show DKIMPass = "pass"
   show DKIMNone = "none"
   show DKIMFail = "fail"
   show DKIMPolicy = "policy"
   show DKIMNeutral = "neutral"
   show DKIMTempError = "temperror"
   show DKIMPermError = "permerror"

-- | See https://tools.ietf.org/html/rfc7208#section-2.6 for what these mean.
-- If you have a SPF policy on your domain, only SPFPass should be considered a success.
data SPFResult = SPFPass | SPFNone | SPFNeutral | SPFFail | SPFSoftFail | SPFTempError | SPFPermError
   deriving (Eq)

instance Read SPFResult where
   readsPrec _ str = readsResult [
      ("pass", SPFPass),
      ("none", SPFNone),
      ("fail", SPFFail),
      ("neutral", SPFNeutral),
      ("softfail", SPFSoftFail),
      ("temperror", SPFTempError),
      ("permerror", SPFPermError)] str

instance Show SPFResult where
   show SPFPass = "pass"
   show SPFNone = "none"
   show SPFFail = "fail"
   show SPFNeutral = "neutral"
   show SPFSoftFail = "softfail"
   show SPFTempError = "temperror"
   show SPFPermError = "permerror"

readsResult possibleMatches str = catMaybes $ map matches possibleMatches
   where
      matches (prefix, val) = let (pre, rest) = splitAt (length prefix) str
                              in case prefix == pre of
                                      True -> Just (val, rest)
                                      False -> Nothing
