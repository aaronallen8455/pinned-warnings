{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Internal.FixWarnings
  ( fixWarning
  , fixRedundancyWarning
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import           Data.Monoid (Alt(..))
import qualified Data.Map.Strict as M
import qualified System.Directory as Dir
import qualified Text.ParserCombinators.ReadP as P

import qualified Internal.GhcFacade as Ghc
import           Internal.Types

-- | Fixes applicable warning and returns 'False' if all warnings for the
-- corresponding span should be removed.
fixWarning :: WarningsWithModDate -> IO WarningsWithModDate
fixWarning (Alt (Just modifiedAt), MonoidMap warnMap) = do
  -- State is used to keep the contents of a source file in memory while all
  -- applicable warnings for that file are fixed.
  (pairs, files) <- (`runStateT` M.empty)
           . flip filterM (reverse $ M.toList warnMap) $ \case
    ((start, _), warnSet)
      | Alt (Just reWarn) -- Take the first redundancy warning parsed
          <- foldMap (Alt . parseRedundancyWarn) warnSet
      -> do
        let file = Ghc.unpackFS $ Ghc.srcLocFile start

        mCached <- gets (M.lookup file)

        srcLines <-
          maybe (liftIO . fmap BS.lines $ BS.readFile file)
                pure
                mCached

        fileModified <- liftIO $ Dir.getModificationTime file
        if fileModified /= modifiedAt
           then do
             -- Do not attempt to edit if file has been touched since last reload
             liftIO . putStrLn
               $ "'" <> file
               <> "' has been modified since last compiled. Reload and try again."
             pure True

           -- attempt to fix the warning
           else do
             let startLine = Ghc.srcLocLine start
                 mNewSrcLines =
                   fixRedundancyWarning startLine reWarn srcLines

             case mNewSrcLines of
               Nothing -> pure True

               Just newSrcLines -> do
                 modify' $ M.insert file newSrcLines

                 pure False

    _ -> pure True

  -- write the changes to the file
  _ <- M.traverseWithKey
         (\file ls -> do
           BS.writeFile file $ BS.unlines ls
           putStrLn $ "'" <> file <> "' has been edited"
         )
         files

  pure (Alt Nothing, MonoidMap $ M.fromList pairs)

fixWarning w = pure w

-- TODO handle operators

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

      isStart bs = (== "import") . BS.take 6 $ BS.dropSpace bs

      (before', stmt')
        | isStart stmt = (before, [stmt])
        | otherwise =
          let (inS, st : rs) = break isStart $ stmt : reverse before
           in (reverse rs, st : reverse inS)

      (stmt'', after') = splitAtImportEnd $ stmt' <> after

   in case warn of
        WholeModule ->
          Just $ before <> after

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
  , isSeparator . BS.take 1 $ BS.drop thingLen match
  , isCellStart . BS.take 1 $ BS.reverse start

  -- check that there isn't a second match
  , (start2, match2) <- BS.breakSubstring thingBS (BS.drop thingLen match)
  , BS.null match2
      || not
         ( isSeparator (BS.take 1 $ BS.drop thingLen match2)
        && isCellStart (BS.take 1 $ BS.reverse start2)
         )

  , let start' = BS.dropWhileEnd (\c -> c /= ',' && c /= '(') start
        end = dropRest
            . BS.dropSpace
            $ BS.drop thingLen match

  = case BS.take 1 end of
      "," -> Just $ start' <> BS.drop 1 end
      ")" -> Just $ BS.take (BS.length start' - 1) start' <> end
      _   -> Nothing

  | otherwise = Nothing
  where
    thingBS = BS.pack thing
    thingLen = BS.length thingBS
    isSeparator c = BS.all isSpace c || c `elem` [",", "(", ")"]
    isCellStart c = BS.all isSpace c || c `elem` [",", "("]
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

