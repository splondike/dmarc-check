module CheckEmailTest where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.Time (CalendarTime, ClockTime(TOD), toUTCTime)
import qualified Data.ByteString.Lazy as LazyByteString (fromStrict, ByteString)
import Data.ByteString.Lazy.Char8 (pack)

import CheckEmail(emailToResult, Result(..))
import EmailExtract(Email(..))
import Config(Config(..))

allTests = testGroup "CheckEmail Tests" [
   testCase "No XML email" testNoXmlErrors,
   testCase "Bad XML email" testInvalidXmlErrors,
   testCase "All good email" testMatchesIpPasses,
   testCase "No ip email" testNoMatchesIpPasses,
   testCase "Bad spf email" testBadSpfFails,
   testCase "Bad dkim email" testBadDkimFails]

testNoXmlErrors = assertEqual "Parse result is Error" Error actual
   where
      actual = emailToResult conf (makeEmail Nothing)

testInvalidXmlErrors = assertEqual "Parse result is Error" Error actual
   where
      actual = emailToResult conf (makeEmail (Just "bad xml"))

testMatchesIpPasses = assertEqual "Parse result is Pass" Pass actual
   where
      actual = emailToResult conf (makeEmail (Just $ makeXml "111.111.111.111" "pass" "pass"))

testNoMatchesIpPasses = assertEqual "Parse result is Pass" Pass actual
   where
      actual = emailToResult conf (makeEmail (Just $ makeXml "111.111.111.112" "fail" "fail"))

testBadSpfFails = assertEqual "Parse result is Fail" Fail actual
   where
      actual = emailToResult conf (makeEmail (Just $ makeXml "111.111.111.111" "fail" "pass"))

testBadDkimFails = assertEqual "Parse result is Fail" Fail actual
   where
      actual = emailToResult conf (makeEmail (Just $ makeXml "111.111.111.111" "pass" "fail"))

makeEmail Nothing = Email "" theTime Nothing
makeEmail (Just str) = Email "" theTime (Just (pack str))

makeXml ip spf dkim = "\
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

conf = Config ["111.111.111.111"] "" "" "" ""
theTime = toUTCTime $ TOD 0 0
