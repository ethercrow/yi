{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Substitute
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.Substitute (parse) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable (asum)
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Buffer.Adjusted hiding (Delete)
import           Yi.Editor
import           Yi.MiniBuffer
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types
import qualified Yi.Rope as R
import           Yi.Search

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    percents <- P.many (P.char '%')
    void $ P.try (P.string "substitute") <|> P.string "s"
    delimiter <- P.oneOf "!@#$%^&*()[]{}<>/.,~';:?-="
    from <- R.fromString <$> P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    to <- R.fromString <$> P.many (P.noneOf [delimiter])
    void $ P.char delimiter
    flagChars <- P.many (P.oneOf "gic")
    return $! substitute from to delimiter
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        ('c' `elem` flagChars)
        (not $ null percents)

substitute :: R.YiString -> R.YiString -> Char -> Bool -> Bool -> Bool -> Bool -> ExCommand
substitute from to delimiter global caseInsensitive confirm allLines = Common.pureExCommand {
    cmdShow = (if allLines then "%" else "")
              <>       "substitute"
              <>       (delimiter `T.cons` R.toText from)
              <>       (delimiter `T.cons` R.toText to)
              `T.snoc` delimiter
              <>       (if confirm then "c" else "")
              <>       (if caseInsensitive then "i" else "")
              <>       (if global then "g" else "")
  , cmdAction = EditorA $ do
        regex <- if R.null from
                    then getRegexE
                    else return . Just . makeSimpleSearch $ from
        case regex of
            Nothing -> printMsg "No previous search pattern"
            Just regex' -> if confirm
                then substituteConfirm regex' to global allLines
                else withCurrentBuffer $ do
                    let replace = void $ regionOfB Line
                                  >>= searchAndRepRegion0 regex' to global
                    if allLines
                        then withEveryLineB replace
                        else replace
                    moveToSol
  }

-- | Run substitution in confirm mode
substituteConfirm :: SearchExp -> R.YiString -> Bool -> Bool -> EditorM ()
substituteConfirm regex to global allLines = do
    setRegexE regex
    regions <- withCurrentBuffer $ findMatches regex global allLines
    substituteMatch to 0 False regions

-- | All matches to replace under given flags
findMatches :: SearchExp -> Bool -> Bool -> BufferM [Region]
findMatches regex global allLines = do
    lns <- if allLines
        then do lineCount <- lineCountB
                lineRegions [1..lineCount]
        else return <$> regionOfB Line
    let f = if global then id else take 1
    concat <$> mapM (fmap f . regexRegionB regex) lns

-- | Get regions corresponding to all lines
lineRegions :: [Int] -> BufferM [Region]
lineRegions = mapM $ \ln -> gotoLn ln >> regionOfB Line

-- | Offsets a region (to account for a region prior being modified)
offsetRegion :: Int -> Region -> Region
offsetRegion k reg = mkRegion (regionStart reg + k') (regionEnd reg + k')
    where k' = fromIntegral k

-- | Runs a list of matches using itself as a continuation
substituteMatch :: R.YiString -> Int -> Bool -> [Region] -> EditorM ()
substituteMatch _ _ _ [] = resetRegexE
substituteMatch to co autoAll (m:ms) = do
    let m' = offsetRegion co m
    withCurrentBuffer . moveTo $ regionStart m'
    len <- withCurrentBuffer $ R.length <$> readRegionB m'
    let diff = R.length to - len
        tex = "replace with " <> R.toText to <> " (y/n/a/q)?"
    if autoAll
        then do withCurrentBuffer $ replaceRegionB m' to
                substituteMatch to (co + diff) True ms
        else void . spawnMinibufferE tex . const $ askKeymap to co (co + diff) m ms

-- | Actual choices during confirm mode.
askKeymap :: R.YiString -> Int -> Int -> Region -> [Region] -> Keymap
askKeymap to co co' m ms = asum
    [ char 'n' ?>>! cleanUp >> substituteMatch to co False ms
    , char 'a' ?>>! do cleanUp
                       replace
                       substituteMatch to co' True ms
    , char 'y' ?>>! do cleanUp
                       replace
                       substituteMatch to co' False ms
    , char 'q' ?>>! cleanUp >> resetRegexE
    ]
    where cleanUp = closeBufferAndWindowE
          replace = withCurrentBuffer $ replaceRegionB (offsetRegion co m) to
