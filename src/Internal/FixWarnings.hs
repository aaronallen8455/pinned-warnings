{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Data.Char (isSpace)
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid (Alt(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.Syntax as Syn
import qualified System.Directory as Dir
import qualified Text.ParserCombinators.ReadP as P

import qualified Internal.GhcFacade as Ghc
import           Internal.Types

-- | Fixes applicable warning
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
    parseResult <- EP.parseModule Paths.libdir modFile

    case parseResult of
      Left _ -> do
        putStrLn $ "Failed to parse module: " <> modFile
        pure warns
      Right (EP.makeDeltaAst ->
              Ghc.L modLoc hsMod@Syn.HsModule{Syn.hsmodImports = imports}) -> do

        -- State is used to keep the contents of the source file in memory while
        -- warnings for the file are fixed.
        (pairs, newImports) <- (`runStateT` imports)
                   -- filter out the warnings that were corrected
                 . flip filterM (reverse $ M.toList warnMap) $ \case
          -- TODO don't need src loc data anymore
          ((_, _), warnSet)
            | Alt (Just reWarn) -- Take the first redundancy warning parsed
                <- foldMap (Alt . parseRedundancyWarn) warnSet
            -> do
              importDecls <- get
              case fixRedundancyWarning reWarn importDecls of
                Nothing -> pure True
                Just newImportDecls -> do
                  put newImportDecls
                  pure False

          _ -> pure True

        when (length pairs /= length warnMap) $ do
          -- write the changes to the file
          writeFile modFile $
            EP.exactPrint (Ghc.L modLoc hsMod { Syn.hsmodImports = newImports })
          putStrLn $ "'" <> modFile <> "' has been edited"

        pure MkWarningsWithModDate
               { lastUpdated = lastModification
               , warningsMap = MonoidMap $ M.fromList pairs
               }

-- | Attempt to fix redundant import warning.
-- Returns 'Nothing' if incapable of fixing.
fixRedundancyWarning :: RedundancyWarn
                     -> [Syn.LImportDecl Ghc.GhcPs]
                     -> Maybe [Syn.LImportDecl Ghc.GhcPs]
fixRedundancyWarning (WholeModule modName) imports =
  case List.partition (matchModule modName) imports of
    (_:_, rest) -> Just rest
    _ -> Nothing
fixRedundancyWarning (IndividualThings things modName) imports = do
  (before, Ghc.L impLoc matchedImp : after)
    <- Just $ break (matchModule modName) imports
  (Syn.Exactly, Ghc.L ieLoc lies) <- Syn.ideclImportList matchedImp

  importResults <- traverse (removeRedundantThings things) lies

  let efNames = effectedNames importResults
  -- Assert that a thing didn't occur in two or more places and is therefore ambiguous
  guard $ Set.size (Set.fromList efNames) == length efNames

  let newLies = transferLeadingAnchor importResults lies
              . removeTrailingComma importResults
              $ mapMaybe unwrapResult importResults

  Just $ before
      ++ Ghc.L impLoc matchedImp
           { Syn.ideclImportList = Just (Syn.Exactly, Ghc.L ieLoc newLies) }
       : after

-- remove trailing comma from the last IE if the previous last was removed
removeTrailingComma
  :: [ImportResult (Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn Ghc.AnnListItem)) a)]
  -> [Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn Ghc.AnnListItem)) a]
  -> [Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn Ghc.AnnListItem)) a]
removeTrailingComma importResults ls
  | Effected _ Nothing : _ <- take 1 $ reverse importResults
  , Ghc.L srcSpan ie : rest <- take 1 $ reverse ls
  = let newSrcSpan = srcSpan
          { Ghc.ann = case Ghc.ann srcSpan of
              Ghc.EpAnn entry _ comments ->
                Ghc.EpAnn
                  { Ghc.entry = entry
                  , Ghc.anns = Ghc.AnnListItem {Ghc.lann_trailing = []}
                  , Ghc.comments = comments
                  }
              Ghc.EpAnnNotUsed -> Ghc.EpAnnNotUsed
          }
     in reverse $ Ghc.L newSrcSpan ie : rest
  | otherwise = ls

transferLeadingAnchor
  :: [ImportResult (Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn ann)) a)]
  -> [Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn ann)) a]
  -> [Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn ann)) a]
  -> [Ghc.GenLocated (Ghc.SrcSpanAnn' (Ghc.EpAnn ann)) a]
transferLeadingAnchor importResults beforeTransform ls
  | Effected _ Nothing : _ <- take 1 importResults
  , Ghc.L srcSpan ie : rest <- take 1 ls
  , Ghc.L oldSrcSpan _ : _ <- take 1 beforeTransform
  = let newSrcSpan = srcSpan
          { Ghc.ann = case Ghc.ann srcSpan of
              Ghc.EpAnn e anns comments ->
                Ghc.EpAnn { Ghc.entry = case Ghc.ann oldSrcSpan of
                                          Ghc.EpAnn entry _ _ -> entry
                                          _ -> e
                          , Ghc.anns = anns
                          , Ghc.comments = comments
                          }
              Ghc.EpAnnNotUsed -> Ghc.EpAnnNotUsed
          }
     in Ghc.L newSrcSpan ie : rest
  | otherwise = ls

matchModule :: String -> Syn.LImportDecl Ghc.GhcPs -> Bool
matchModule modName
  = (== modName) . Syn.moduleNameString
  . Ghc.unLoc . Syn.ideclName . Ghc.unLoc

data ImportResult a
  = NoOp a
  | Effected [String] (Maybe a)

unwrapResult :: ImportResult a -> Maybe a
unwrapResult = \case
  NoOp i -> Just i
  Effected _ i -> i

effectedNames :: [ImportResult a] -> [String]
effectedNames rs = (`concatMap` rs) $ \case
  NoOp _ -> []
  Effected ns _ -> ns

removeRedundantThings :: [String] -> Syn.LIE Ghc.GhcPs -> Maybe (ImportResult (Syn.LIE Ghc.GhcPs))
removeRedundantThings things lie@(Ghc.L ieLoc ie) =
  let nameMatches = case ie of
        Syn.IEVar _ (Ghc.L _ (Ghc.occName -> occN)) -> do
          guard $ Ghc.occNameString occN `elem` things
          Just $ Ghc.occNameString occN
        Syn.IEThingAbs _ (Ghc.L _ (Ghc.occName -> occN)) -> do
          guard $ Ghc.occNameString occN `elem` things
          Just $ Ghc.occNameString occN
        Syn.IEThingAll _ (Ghc.L _ (Ghc.occName -> occN)) -> do
          guard $ Ghc.occNameString occN `elem` things
          Just $ Ghc.occNameString occN
        Syn.IEThingWith _ (Ghc.L _ (Ghc.occName -> occN)) _ _ -> do
          guard $ Ghc.occNameString occN `elem` things
          Just $ Ghc.occNameString occN
        Syn.IEModuleContents{} -> Nothing
        Syn.IEGroup{} -> Nothing
        Syn.IEDoc{} -> Nothing
        Syn.IEDocNamed{} -> Nothing
      matchedAssocThing = case ie of
        Syn.IEThingWith x n w assocWrappedNames -> do -- Maybe
          let results = do -- List
                assocName <- assocWrappedNames
                let nameStr = Ghc.occNameString . Ghc.occName $ Ghc.unLoc assocName
                pure $ if nameStr `elem` things
                   then Effected [nameStr] Nothing
                   else NoOp assocName
              newAssocNames = mapMaybe unwrapResult results
          guard $ length newAssocNames /= length assocWrappedNames
          if null newAssocNames
             then Just (Syn.IEThingAbs x n, effectedNames results)
             else Just
                . (, effectedNames results)
                . Syn.IEThingWith x n w
                . transferLeadingAnchor results assocWrappedNames
                $ removeTrailingComma results newAssocNames

        _ -> Nothing
   in case (nameMatches, matchedAssocThing) of
        -- Don't allow ambiguous occurences of the identifier, i.e. both a data con and a type
        (Just _, Just _) -> Nothing
        (Just n, _) -> Just $ Effected [n] Nothing
        (_, Just (newIE, ns)) -> Just . Effected ns $ Just (Ghc.L ieLoc newIE)
        (Nothing, Nothing) -> Just $ NoOp lie

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- TODO use structured diagnostics instead of parsing error messages

-- | Redundant import warnings
data RedundancyWarn
  = WholeModule
      String -- ^ module name
  | IndividualThings
      [String] -- ^ redundant things
      String -- ^ module name
  deriving Show

parseRedundancyWarn :: Warning -> Maybe RedundancyWarn
parseRedundancyWarn warn =
  case P.readP_to_S redundancyWarnParser (showWarning warn) of
    [(w, "")] -> Just w
    _ -> Nothing

redundancyWarnParser :: P.ReadP RedundancyWarn
redundancyWarnParser = do
  _ <- P.string "The import of ‘"
   <|> P.string "The qualified import of ‘"

  inQuotes@(firstThing : _) <-
    P.sepBy1 (P.munch1 $ \c -> not (isSpace c) && c `notElem` [',', '’'])
             (P.char ',' <* P.skipSpaces)

  _ <- P.char '’'

  let terms
        = IndividualThings inQuotes
         <$> ( P.skipSpaces
            *> P.string "from module ‘"
            *> P.munch1 (/= '’')
            <* P.string "’ is redundant"
             )

      wholeMod = WholeModule firstThing
              <$ (P.skipSpaces *> P.string "is redundant")

  result <- P.choice [terms, wholeMod]

  _ <- P.munch (const True)

  pure result
