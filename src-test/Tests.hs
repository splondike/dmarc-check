import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)

import qualified DMARCAggregateReportTest as DMARCAggregateReportTest (allTests) 

tests = [
   DMARCAggregateReportTest.allTests
   ]

main = defaultMain tests
