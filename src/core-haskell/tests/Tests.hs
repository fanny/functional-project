module Tests (
  runTests
) where

import TestFilters
import TestValues
import TestBalances
import TestAverages
import TestCashFlows
import Test.HUnit

runTests = do
  runTestTT testFilters
  runTestTT testValues
  runTestTT testBalances
  runTestTT testAverages
  runTestTT testCashFlows