{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import qualified GHC.Paths as Paths
import qualified GHC.Types.SrcLoc as Ghc
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.Syntax as Syn
import           Test.Tasty
import           Test.Tasty.HUnit

import           Internal.FixWarnings

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "FixWarnings"
  [ testCase "Removes thing" $ do
      doTest input1 (IndividualThings ["foo"] "Foo.Bar") output1

      doTest input2 (IndividualThings ["bar"] "Foo.Bar") output2

      doTest input3 (IndividualThings ["baz", "foo"] "Foo.Bar") output3

      doTest input4 (IndividualThings ["bar"] "Foo.Bar") output4

      doTest input5 (IndividualThings ["foo", "baz"] "Foo.Bar") output5

      doTest input6 (IndividualThings ["bar"] "Foo.Bar") output6

      doTest input7 (IndividualThings ["Bar"] "Foo.Bar") output7

      doTest input8 (IndividualThings ["Bar"] "Foo") output8

      doTest input9 (IndividualThings ["two"] "Foo") output9

      doTest input9 (IndividualThings ["one"] "Foo") output9a

      doTest input10 (IndividualThings ["+~"] "Foo") output10

      doTest input11 (IndividualThings [":~:"] "Foo") output11

      doTest input12 (IndividualThings ["two"] "Foo") output12

      doTest input12 (IndividualThings ["one"] "Foo") output12a

      doTest input13 (WholeModule "Foo") output13

      doTest input14 (WholeModule "Foo") output14

      doTest input15 (WholeModule "Foo") output15

      doTest input16 (IndividualThings ["zip", "zipWith"] "Data.List") output16

      doTest input17 (IndividualThings ["Baz"] "Foo") output17

      doTest input18 (IndividualThings ["+"] "Foo") output18

      doTest input19 (IndividualThings [":+:"] "Foo") output19

      doTest input20 (IndividualThings ["Foo"] "Foo") output20

      doTest input21 (IndividualThings [":|"] "Data.List.NonEmpty") output21

      doTest input22 (IndividualThings ["zip"] "Data.List") output22

      doTest input21 (IndividualThings ["NonEmpty"] "Data.List.NonEmpty") output21a

      doTest input23 (IndividualThings [":|"] "Data.List.NonEmpty") output23

      doTest input24 (IndividualThings ["foo"] "Foo") output24

      doTest input25 (IndividualThings ["foo"] "Foo") output25

      doTest input26 (IndividualThings ["Foo"] "Foo") output26

      doTest input27 (IndividualThings ["Foo"] "Foo") output27
  ]

doTest :: String -> RedundancyWarn -> String -> IO ()
doTest input warn (BS.strip . BS.pack -> output) =
  (@?= output) . BS.strip . BS.pack
    =<< applyFix input warn

applyFix :: String -> RedundancyWarn -> IO String
applyFix input warn = do
  parseResult <- EP.parseModuleFromString Paths.libdir "" input
  case parseResult of
    Left _ -> fail $ "parse failed: " <> input
    Right (EP.makeDeltaAst ->
            Ghc.L modLoc hsMod@Syn.HsModule{Syn.hsmodImports = imports}) ->
      let mNewImports = fixRedundancyWarning warn imports
       in pure $
            maybe
              input
              (\newImports -> EP.exactPrint (Ghc.L modLoc hsMod { Syn.hsmodImports = newImports }))
              mNewImports

input1, output1 :: String
input1  = "import Foo.Bar (foo, bar)"
output1 = "import Foo.Bar (bar)"

input2, output2 :: String
input2  = "import Foo.Bar (foo, bar)"
output2 = "import Foo.Bar (foo)"

input3, output3 :: String
input3  = "import Foo.Bar (foo, bar, baz)"
output3 = "import Foo.Bar (bar)"

input4, output4 :: String
input4  = "import Foo.Bar (foo, bar, baz)"
output4 = "import Foo.Bar (foo, baz)"

input5, output5 :: String
input5  = unlines
          [ "import Foo.Bar ( foo"
          , "               , bar"
          , "               , baz"
          , "               )"
          ]
output5 = unlines
          [ "import Foo.Bar ( bar"
          , "               )"
          ]

input6, output6 :: String
input6  = unlines
          [ "import Foo.Bar ( foo"
          , "               , bar"
          , "               , baz"
          , "               )"
          ]
output6 = unlines
          [ "import Foo.Bar ( foo"
          , "               , baz"
          , "               )"
          ]

input7, output7 :: String
input7  = unlines
          [ "{-# LANGUAGE PatternSynonyms #-}"
          , "import Foo.Bar"
          , "  ( foo"
          , "  , pattern Bar"
          , "  , baz"
          , "  )"
          ]
output7 = unlines
          [ "{-# LANGUAGE PatternSynonyms #-}"
          , "import Foo.Bar"
          , "  ( foo"
          , "  , baz"
          , "  )"
          ]

input8, output8 :: String
input8  = "import Foo (foo, Bar(one, two), baz)"
output8 = "import Foo (foo, baz)"

input9, output9 :: String
input9  = "import Foo (foo, Bar(one, two), baz)"
output9 = "import Foo (foo, Bar(one), baz)"
output9a = "import Foo (foo, Bar(two), baz)"

input10, output10 :: String
input10  = "import Foo (foo, (+~), baz)"
output10 = "import Foo (foo, baz)"

input11, output11 :: String
input11  = "import Foo (foo, (:~:)(..), baz)"
output11 = "import Foo (foo, baz)"

input12, output12 :: String
input12  = "import Foo (foo, (:~:)(one, two), baz)"
output12 = "import Foo (foo, (:~:)(one), baz)"
output12a = "import Foo (foo, (:~:)(two), baz)"

input13, output13 :: String
input13 = unlines
          [ "import Foo"
          , "  (one, two, three)"
          , "import Bar"
          ]
output13 = "import Bar"

input14, output14 :: String
input14 = unlines
          [ "import Foo (one"
          , "           ,two"
          , "           )"
          , "import Bar"
          ]
output14 = "import Bar"

input15, output15 :: String
input15 = unlines
          [ "import Foo"
          , "import Bar"
          ]
output15 = "import Bar"

-- first match is a subsubstring of another term
input16, output16 :: String
input16 = "import Data.List (zip4, zip, zipWith3, zipWith, zipWith2, zip3)"
output16 = "import Data.List (zip4, zipWith3, zipWith2, zip3)"

input17, output17 :: String
input17 = "import Foo (Bar( Baz), foo)"
output17 = "import Foo (Bar, foo)"

input18, output18 :: String
input18 = "import Foo (foo, ( +  ))"
output18 = "import Foo (foo)"

input19, output19 :: String
input19 = "import Foo (foo, ( :+:  ) (.. ), bar)"
output19 = "import Foo (foo, bar)"

input20, output20 :: String
input20 = "import Foo (foo, Foo (.. ))"
output20 = "import Foo (foo)"

input21, output21 :: String
input21 = "import Data.List.NonEmpty (NonEmpty((:|)), foo)"
output21 = "import Data.List.NonEmpty (NonEmpty, foo)"
output21a = "import Data.List.NonEmpty (foo)"

input22, output22 :: String
input22 = "import Data.List (zipzip, zip)"
output22 = "import Data.List (zipzip)"

input23, output23 :: String
input23 = "import Data.List.NonEmpty (NonEmpty((:|), bar), foo)"
output23 = "import Data.List.NonEmpty (NonEmpty(bar), foo)"

input24, output24 :: String
input24 = "import Foo (Foo(foo))"
output24 = "import Foo (Foo)"

input25, output25 :: String
input25 = "import Foo (Foo(foo), bar)"
output25 = "import Foo (Foo, bar)"

input26, output26 :: String
input26 = "import Foo (Foo, Bar(Foo))"
output26 = "import Foo (Foo, Bar(Foo))"

input27, output27 :: String
input27 = "import Foo (Foo(Foo))"
output27 = "import Foo (Foo(Foo))"
