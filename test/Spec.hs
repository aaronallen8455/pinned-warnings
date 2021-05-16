{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import           Test.Tasty
import           Test.Tasty.HUnit

import           Internal.FixWarnings

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "FixWarnings"
  [ testCase "Removes thing" $ do
      fixRedundancyWarning 1 (IndividualThings ["foo"]) [input1]
        @?= Just [output1]

      fixRedundancyWarning 1 (IndividualThings ["bar"]) [input2]
        @?= Just [output2]

      fixRedundancyWarning 1 (IndividualThings ["baz", "foo"]) [input3]
        @?= Just [output3]

      fixRedundancyWarning 1 (IndividualThings ["bar"]) [input4]
        @?= Just [output4]

      fixRedundancyWarning 1 (IndividualThings ["foo", "baz"]) input5
        @?= Just output5

      fixRedundancyWarning 2 (IndividualThings ["bar"]) input6
        @?= Just output6

      fixRedundancyWarning 3 (IndividualThings ["bar"]) input7
        @?= Just output7

      fixRedundancyWarning 1 (IndividualThings ["Bar"]) [input8]
        @?= Just [output8]

      fixRedundancyWarning 1 (IndividualThings ["two"]) [input9]
        @?= Just [output9]

      fixRedundancyWarning 1 (IndividualThings ["+~"]) [input10]
        @?= Just [output10]

      fixRedundancyWarning 1 (IndividualThings [":~:"]) [input11]
        @?= Just [output11]

      fixRedundancyWarning 1 WholeModule input13
        @?= Just output13

      fixRedundancyWarning 1 WholeModule input14
        @?= Just output14

      fixRedundancyWarning 1 WholeModule input15
        @?= Just output15
  ]

input1, output1 :: BS.ByteString
input1  = "import Foo.Bar (foo, bar)"
output1 = "import Foo.Bar (bar)"

input2, output2 :: BS.ByteString
input2  = "import Foo.Bar (foo, bar)"
output2 = "import Foo.Bar (foo)"

input3, output3 :: BS.ByteString
input3  = "import Foo.Bar (foo, bar, baz)"
output3 = "import Foo.Bar (bar)"

input4, output4 :: BS.ByteString
input4  = "import Foo.Bar (foo, bar, baz)"
output4 = "import Foo.Bar (foo, baz)"

input5, output5 :: [BS.ByteString]
input5  = [ "import Foo.Bar ( foo"
          , "               , bar"
          , "               , baz"
          , "               )"
          ]
output5 = [ "import Foo.Bar ( bar"
          , "               )"
          ]

input6, output6 :: [BS.ByteString]
input6  = [ "import Foo.Bar ( foo"
          , "               , bar"
          , "               , baz"
          , "               )"
          ]
output6 = [ "import Foo.Bar ( foo"
          , "               , baz"
          , "               )"
          ]

input7, output7 :: [BS.ByteString]
input7  = [ "import Foo.Bar"
          , "  ( foo"
          , "  , bar"
          , "  , baz"
          , "  )"
          ]
output7 = [ "import Foo.Bar"
          , "  ( foo"
          , "  , baz"
          , "  )"
          ]

input8, output8 :: BS.ByteString
input8  = "import Foo (foo, Bar(one, two), baz)"
output8 = "import Foo (foo, baz)"

input9, output9 :: BS.ByteString
input9  = "import Foo (foo, Bar(one, two), baz)"
output9 = "import Foo (foo, Bar(one), baz)"

input10, output10 :: BS.ByteString
input10  = "import Foo (foo, (+~), baz)"
output10 = "import Foo (foo, baz)"

input11, output11 :: BS.ByteString
input11  = "import Foo (foo, (:~:)(..), baz)"
output11 = "import Foo (foo, baz)"

input12, output12 :: BS.ByteString
input12  = "import Foo (foo, (:~:)(one, two), baz)"
output12 = "import Foo (foo, (:~:)(one), baz)"

input13, output13 :: [BS.ByteString]
input13 = [ "import Foo"
          , "  (one, two, three)"
          , "import Bar"
          ]
output13 = [ "import Bar" ]

input14, output14 :: [BS.ByteString]
input14 = [ "import Foo (one"
          , "           ,two"
          , "           )"
          , "import Bar"
          ]
output14 = [ "import Bar" ]

input15, output15 :: [BS.ByteString]
input15 = [ "import Foo"
          , "import Bar"
          ]
output15 = [ "import Bar" ]
