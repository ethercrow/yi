{-# LANGUAGE FlexibleContexts, TemplateHaskell, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- uniplate uses incomplete patterns
-- Copyright (c) Jean-Philippe Bernardy 2008

module Yi.Regex (
    SearchOption(..)
  , makeSearchExp
  , SearchExp(..)
  , emptySearchExp
  , emptyRegex
  , regexEscapeString

  -- reexports from Text.Regex
  , Regex
  , matchAll
  , matchOnceText
  , makeRegex
  , (=~)
  , AllTextSubmatches(..)
  ) where

import Data.Binary
import Data.DeriveTH
import System.IO.Unsafe (unsafePerformIO)

import Text.Regex.PCRE
import Text.Regex.PCRE.String

-- input string, regexexp, backward regex.
data SearchExp = SearchExp {
    searchString :: String
  , seCompiled   :: Regex
  , seOptions    :: [SearchOption]
  }

instance Show SearchExp where
    show (SearchExp input _ opts) = "SearchExp \"" ++ input ++ "\" / " ++ show opts

data SearchOption
    = IgnoreCase   -- ^ Compile for matching that ignores char case
    | NoNewLine    -- ^ Compile for newline-insensitive matching
    | QuoteRegex   -- ^ Treat the input not as a regex but as a literal string to search for.
    deriving (Show, Eq)

$(derive makeBinary ''SearchOption)

makeSearchExp :: [SearchOption] -> String -> Either String SearchExp
makeSearchExp opts patternString =
    fmap regexToSearchExp . mapLeft show . unsafePerformIO $ compile compOpts execOpts patternString
    where compOpts = compBlank
          execOpts = execBlank
          regexToSearchExp re = SearchExp { searchString = patternString
                                          , seCompiled = re
                                          , seOptions = opts
                                          }

instance Binary SearchExp where
  get = do re   <- get
           opts <- get
           return $ case makeSearchExp opts re of
                      Left err -> error err
                      Right se -> se
  put (SearchExp { searchString   = re,
                   seOptions = opts, .. }) = do put re
                                                put opts

mapLeft :: (t1 -> a) -> Either t1 t -> Either a t
mapLeft _ (Right a) = Right a
mapLeft f (Left a) = Left (f a)

emptySearchExp :: SearchExp
emptySearchExp = SearchExp "" emptyRegex []


-- | The regular expression that matches nothing.
emptyRegex :: Regex
Right emptyRegex = unsafePerformIO $ compile compBlank execBlank "a^"

regexEscapeString :: String -> String
regexEscapeString = id -- TODO

