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

  let importResults = removeRedundantThings things <$> lies

  -- Fail if there is not a single import that was modified or removed
  guard $ (== 1) . length $ filter isEffected importResults
  let newLies = mapMaybe unwrapResult importResults
  Just $ before
      ++ Ghc.L impLoc matchedImp
           { Syn.ideclImportList = Just (Syn.Exactly, Ghc.L ieLoc newLies) }
       : after

matchModule :: String -> Syn.LImportDecl Ghc.GhcPs -> Bool
matchModule modName
  = (== modName) . Syn.moduleNameString
  . Ghc.unLoc . Syn.ideclName . Ghc.unLoc

data ImportResult
  = NoOp (Syn.LIE Ghc.GhcPs)
  | Effected (Maybe (Syn.LIE Ghc.GhcPs))
  | Ambiguous

isEffected :: ImportResult -> Bool
isEffected Effected{} = True
isEffected Ambiguous = True
isEffected NoOp{} = False

unwrapResult :: ImportResult -> Maybe (Syn.LIE Ghc.GhcPs)
unwrapResult = \case
  NoOp i -> Just i
  Effected i -> i
  Ambiguous -> Nothing

removeRedundantThings :: [String] -> Syn.LIE Ghc.GhcPs -> ImportResult
removeRedundantThings things lie@(Ghc.L ieLoc ie) =
  let nameMatches = case ie of
        Syn.IEVar _ (Ghc.L _ (Ghc.occName -> occN)) ->
          Ghc.occNameString occN `elem` things
        Syn.IEThingAbs _ (Ghc.L _ (Ghc.occName -> occN)) ->
          Ghc.occNameString occN `elem` things
        Syn.IEThingAll _ (Ghc.L _ (Ghc.occName -> occN)) ->
          Ghc.occNameString occN `elem` things
        Syn.IEThingWith _ (Ghc.L _ (Ghc.occName -> occN)) _ _ ->
          Ghc.occNameString occN `elem` things
        Syn.IEModuleContents{} -> False
        Syn.IEGroup{} -> False
        Syn.IEDoc{} -> False
        Syn.IEDocNamed{} -> False
      matchedAssocThing = case ie of
        Syn.IEThingWith x n w assocWrappedNames -> do -- Maybe
          let assocNameMatches = (`elem` things) . Ghc.occNameString
                               . Ghc.occName . Ghc.unLoc
          (before, _ : rest) <- Just $ break assocNameMatches assocWrappedNames
          Just $ Syn.IEThingWith x n w (before ++ rest)
        _ -> Nothing
   in case (nameMatches, matchedAssocThing) of
        (True, Just _) -> Ambiguous
        (True, _) -> Effected Nothing
        (_, Just newIE) -> Effected $ Just (Ghc.L ieLoc newIE)
        (False, Nothing) -> NoOp lie

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
