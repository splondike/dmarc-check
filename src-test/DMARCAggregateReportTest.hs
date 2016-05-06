module DMARCAggregateReportTest where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Either (isRight)

import Data.String (fromString)
import System.Time (CalendarTime, ClockTime(TOD), toUTCTime)
import qualified Data.ByteString.Lazy as LazyByteString (fromStrict, ByteString)
import Data.ByteString.Lazy.Char8 (pack)

import qualified Data.DMARCAggregateReport as DM

allTests _ = testGroup "DMARCAggregateReport Tests" [
   testCase "Parses valid report" testParsesValid,
   testCase "Bad XML report" testFailsInvalidXml,
   testCase "Bad format report" testFailsInvalidFormat]

testParsesValid = assertEqual "Parse result is success" True (isRight result)
   where
      result = DM.parseReport $ fromString $ makeReport "127.0.0.1" "pass" "pass"

testFailsInvalidXml = assertEqual "Parse result is invalid XML" True (invalidXml result)
   where
      invalidXml (Left DM.XMLParseFailure) = True
      invalidXml _ = False
      result = DM.parseReport $ fromString "invalid xml"

testFailsInvalidFormat = assertEqual "Parse result is invalid format" True (invalidFormat result)
   where
      invalidFormat (Left DM.InvalidDocument) = True
      invalidFormat _ = False
      result = DM.parseReport $ fromString doc
      doc = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?><doc></doc>"

makeReport ip spf dkim = "\
   \<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\
   \<feedback>\
     \<report_metadata>\
       \<org_name>google.com</org_name>\
       \<email>noreply-dmarc-support@google.com</email>\
       \<extra_contact_info>https://support.google.com/a/answer/2466580</extra_contact_info>\
       \<report_id>11234</report_id>\
       \<date_range>\
         \<begin>1439942400</begin>\
         \<end>1440028799</end>\
       \</date_range>\
     \</report_metadata>\
     \<policy_published>\
       \<domain>example.com</domain>\
       \<adkim>r</adkim>\
       \<aspf>r</aspf>\
       \<p>reject</p>\
       \<sp>reject</sp>\
       \<pct>100</pct>\
     \</policy_published>\
     \<record>\
       \<row>\
         \<source_ip>" ++ ip ++ "</source_ip>\
         \<count>2</count>\
         \<policy_evaluated>\
           \<disposition>none</disposition>\
           \<dkim>" ++ dkim ++ "</dkim>\
           \<spf>" ++ spf ++ "</spf>\
         \</policy_evaluated>\
       \</row>\
       \<identifiers>\
         \<header_from>example.com</header_from>\
       \</identifiers>\
       \<auth_results>\
         \<dkim>\
           \<domain>example.com</domain>\
           \<result>pass</result>\
         \</dkim>\
         \<spf>\
           \<domain>example.com</domain>\
           \<result>pass</result>\
         \</spf>\
       \</auth_results>\
     \</record>\
   \</feedback>"
