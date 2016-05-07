import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)

import qualified DMARCAggregateReportTest as DMARCAggregateReportTest (allTests) 
import qualified ParseEmailTest as ParseEmailTest (allTests) 

tests = [
   DMARCAggregateReportTest.allTests dataDir,
   ParseEmailTest.allTests dataDir
   ]

main = defaultMain tests

-- stack test is normally run from the root of the repo, so let's just assume that
dataDir = "src-test/data/"
