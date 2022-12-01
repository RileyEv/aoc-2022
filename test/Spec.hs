import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

import qualified Day1Tests
import qualified Day2Tests
import qualified TemplateTests


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
      testGroup "Tests" [TemplateTests.tests, Day1Tests.tests, Day2Tests.tests]
