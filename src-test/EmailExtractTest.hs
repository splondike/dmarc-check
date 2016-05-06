module EmailExtractTest where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as ByteString

import qualified EmailExtract as EE

allTests dataDir = 
   let testParse' = testParse dataDir
   in testGroup "EmailExtract Tests" [
      testParse' "AOL" "aol-email.txt",
      testParse' "Yahoo" "yahoo-email.txt",
      testParse' "Google" "google-email.txt",
      testParse' "Hotmail" "hotmail-email.txt"]

testParse dataDir companyName emailFilename = testCase testCaseName runTest
   where
      runTest = do
         emailStr <- ByteString.readFile $ dataDir ++ emailFilename
         let maybeEmail = EE.extractEmail emailStr
         assertEqual "Parse result is success" True (isJust maybeEmail)
         let Just email = maybeEmail
         assertEqual "Finds an XML str" True (isJust $ EE.xmlStr email)

      testCaseName = "Parses " ++ companyName ++ " report email"
