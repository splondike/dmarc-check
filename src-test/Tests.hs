import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)

import qualified CheckEmailTest as CheckEmailTest (allTests) 

tests = [
   CheckEmailTest.allTests
   ]

main = defaultMain tests
