{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Internal.FixWarnings
  ( fixWarning
  , fixRedundancyWarning
  , RedundancyWarn(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import           Data.Maybe (isJust)
import           Data.Monoid (Alt(..))
import qualified Data.Map.Strict as M
import qualified System.Directory as Dir
import qualified Text.ParserCombinators.ReadP as P

import qualified Internal.GhcFacade as Ghc
import           Internal.Types

-- | Fixes applicable warning and returns 'False' if all warnings for the
-- corresponding span should be removed.
fixWarning :: ModuleFile -> WarningsWithModDate -> IO WarningsWithModDate
fixWarning modFile
           warns@MkWarningsWithModDate
             { lastUpdated = modifiedAt
             , warningsMap = MonoidMap warnMap
             } = do

  lastModification <- liftIO $ Dir.getModificationTime modFile

  -- Do not attempt to edit if file has been touched since last reload
  if lastModification /= modifiedAt
  then do
    putStrLn
      $ "'" <> modFile
      <> "' has been modified since last compiled. Reload and try again."
    pure warns

  else do
    curSrcLines <- liftIO . fmap BS.lines $ BS.readFile modFile

    -- State is used to keep the contents of the source file in memory while
    -- warnings for the file are fixed.
    (pairs, newFileContents) <- (`runStateT` curSrcLines)
             . flip filterM (reverse $ M.toList warnMap) $ \case
      ((start, _), warnSet)
        | Alt (Just reWarn) -- Take the first redundancy warning parsed
            <- foldMap (Alt . parseRedundancyWarn) warnSet
        -> do
          srcLines <- get

          -- attempt to fix the warning
          let startLine = Ghc.srcLocLine start
              mNewSrcLines =
                fixRedundancyWarning startLine reWarn srcLines

          case mNewSrcLines of
            Nothing -> pure True

            Just newSrcLines -> do
              put newSrcLines
              pure False

      _ -> pure True

    when (length pairs /= length warnMap) $ do
      -- write the changes to the file
      BS.writeFile modFile $ BS.unlines newFileContents
      putStrLn $ "'" <> modFile <> "' has been edited"

    pure MkWarningsWithModDate
           { lastUpdated = lastModification
           , warningsMap = MonoidMap $ M.fromList pairs
           }

-- | Attempt to fix redundant import warning.
-- Returns 'Nothing' if incapable of fixing.
fixRedundancyWarning :: Int
                     -> RedundancyWarn
                     -> [BS.ByteString]
                     -> Maybe [BS.ByteString]
fixRedundancyWarning startLine warn srcLines =
  -- The span for redundant errors is only ever a single line. This means we
  -- must search for the end of the import statement. If this a warning about a
  -- single import thing, the span line may not encompass the start of the
  -- import statement so we must search for that as well.

  let (before, stmt : after) = splitAt (startLine - 1) srcLines

      isStart bs = "import" `BS.isPrefixOf` BS.dropSpace bs

      (before', stmt')
        | isStart stmt = (before, [stmt])
        | otherwise =
          let (inS, st : rs) = break isStart $ stmt : reverse before
           in (reverse rs, st : reverse inS)

      (stmt'', after') = splitAtImportEnd $ stmt' <> after

      hasExplicitList
        -- Check the next line to see if contains an explicit import list
        | a : _ <- after
        , BS.length (BS.takeWhile isSpace a)
            > BS.length (BS.takeWhile isSpace stmt)
        , BS.take 1 (BS.dropSpace a) == "("
          = True
        | otherwise = isJust (BS.elemIndex '(' stmt)

   in case warn of
        WholeModule
          | hasExplicitList -> Just $ before <> after'
          | otherwise       -> Just $ before <> after

        IndividualThings things ->
          (<> after') . (before' <>) . BS.lines <$>
            foldM fixRedundantThing
                  (BS.unlines stmt'')
                  things

splitAtImportEnd :: [BS.ByteString] -> ([BS.ByteString], [BS.ByteString])
splitAtImportEnd ls = first reverse $ go 0 0 ([], ls) where
  go o c acc
    | o /= 0 , o == c
    = acc
  go _ _ acc@(_, []) = acc -- shouldn't happen
  go o c (stmt, r:rest) =
    let addO = length $ BS.elemIndices '(' r
        addC = length $ BS.elemIndices ')' r
     in go (o + addO) (c + addC) (r : stmt, rest)

-- | Removes a particular thing from an import list without disrupting the
-- formatting. Returns 'Nothing' if the thing doesn't exist or appears more
-- than once.
--
-- Edges cases not handled:
-- - Comments interspersed in the statement that mention the thing
-- - Semicolon layout
fixRedundantThing :: BS.ByteString -> String -> Maybe BS.ByteString
fixRedundantThing stmt thing
  | (start, match) <- BS.breakSubstring thingBS stmt
  , not (BS.null match)
  , isSeparator $ BS.drop thingLen match
  , isCellStart $ BS.reverse start

  -- check that there isn't a second valid match
  , (start2, match2) <- BS.breakSubstring thingBS (BS.drop thingLen match)
  , BS.null match2
      || not
         ( isSeparator (BS.drop thingLen match2)
        && isCellStart (BS.reverse start2)
         )

    -- preserve the whitespace immediately after the ',' or '('
  , let start' = let (s, e) = BS.breakEnd (`elem` [',', '(']) start
                  in s <> BS.takeWhile isSpace e

        end = dropRest
            . BS.dropSpace
            $ BS.drop thingLen match

  = BS.uncons end >>= \case
      (',', end') -> Just $ start' <> BS.dropSpace end'
      -- If bound on the right by ')', remove the suffix containing ',' from start
      (')', _) -> Just $ BS.init (BS.dropWhileEnd isSpace start') <> end
      _ -> Nothing

  | otherwise = Nothing
  where
    thingBS | isOperator bs = "(" <> bs <> ")"
            | otherwise = bs
            where bs = BS.pack thing
    thingLen = BS.length thingBS

    isSeparator = headPred $ \c -> isSpace c || c `elem` [',', '(', ')']
    isCellStart = headPred $ \c -> isSpace c || c `elem` [',', '(']
    isOperator = headPred (`elem` opChars)
        where
          opChars :: String
          opChars = ":!#$%&*+./<=>?@\\^|-~"
    headPred :: (Char -> Bool) -> BS.ByteString -> Bool
    headPred p = maybe False (p . fst) . BS.uncons

    -- Remove the portion to the right of the match up to the cell terminator
    dropRest bs = case BS.uncons bs of
                    Nothing -> ""
                    -- Constructors of a type or methods of a class
                    Just (c, r)
                      | c == '(' -> BS.dropWhile (\x -> x /= ',' && x /= ')')
                                  . BS.drop 1
                                  $ BS.dropWhile (/= ')') r

                    _ -> BS.dropSpace bs

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Redundant import warnings
data RedundancyWarn
  = WholeModule
  | IndividualThings [String]
  deriving Show

parseRedundancyWarn :: Warning -> Maybe RedundancyWarn
parseRedundancyWarn (Warning warn) =
  case P.readP_to_S redundancyWarnParser (show warn) of
    [(w, "")] -> Just w
    _ -> Nothing

redundancyWarnParser :: P.ReadP RedundancyWarn
redundancyWarnParser = do
  _ <- P.string "The import of ‘"
   <|> P.string "The qualified import of ‘"

  inQuotes <-
    P.sepBy1 (P.munch1 $ \c -> not (isSpace c) && c /= ',' && c /= '’')
             (P.char ',' <* P.skipSpaces)

  _ <- P.char '’'

  let terms
        = IndividualThings inQuotes
         <$ ( P.skipSpaces
           *> P.string "from module ‘"
           *> P.munch1 (/= '’')
           *> P.string "’ is redundant"
            )

      wholeMod = WholeModule
              <$ (P.skipSpaces *> P.string "is redundant")

  result <- P.choice [terms, wholeMod]

  _ <- P.munch (const True)

  pure result
