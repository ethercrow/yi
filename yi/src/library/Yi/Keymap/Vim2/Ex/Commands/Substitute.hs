module Yi.Keymap.Vim2.Ex.Commands.Substitute
    ( parse
    ) where

import Prelude ()
import Yi.Prelude hiding (from, to)

import qualified Text.ParserCombinators.Parsec as P

import Yi.Buffer hiding (Delete)
import Yi.Keymap
import Yi.Keymap.Vim2.Ex.Types
import qualified Yi.Keymap.Vim2.Ex.Commands.Common as Common
import Yi.Regex
import Yi.Search

parse :: String -> Maybe ExCommand
parse = Common.parse $ do
    range <- Common.parseRange
    discard $ P.try (P.string "substitute") <|> P.string "s"
    delimiter <- P.oneOf "!@#$%^&*()[]{}<>/.,~';:?-="
    from <- P.many (P.noneOf [delimiter])
    discard $ P.char delimiter
    to <- P.many (P.noneOf [delimiter])
    discard $ P.char delimiter
    flagChars <- P.many (P.oneOf "gi")
    return $! substitute from to delimiter
        ('g' `elem` flagChars)
        ('i' `elem` flagChars)
        range

substitute :: String -> String -> Char -> Bool -> Bool -> LineRange -> ExCommand
substitute from to delimiter global caseInsensitive range = Common.pureExCommand {
    cmdShow = concat
        [ show range
        , "substitute" , delimiter : from , delimiter : to , [delimiter]
        , if caseInsensitive then "i" else ""
        , if global then "g" else ""
        ]
  , cmdAction = BufferA $ do
        let opts = [IgnoreCase | caseInsensitive]
            regex = makeSearchExp opts from
            replace = case regex of
                Right se -> do
                    region <- regionOfB Line
                    discard $ searchAndRepRegion0 se to global region
                Left _ -> return ()

        case range of
            LineRange LRBCurrentLine LRBCurrentLine -> replace
            LineRange b1 b2 -> do
              l1 <- lineFromBoundary b1
              l2 <- lineFromBoundary b2
              withLinesInRangeB (l1, l2) replace

        moveToSol
  }

lineFromBoundary :: LineRangeBoundary -> BufferM Int
lineFromBoundary (LRBLineNumber i) = return i
lineFromBoundary LRBEOF = savingPointB $ botB >> curLn
lineFromBoundary LRBCurrentLine = curLn
lineFromBoundary (LRBMark markName) = savingPointB $ do
    mmark <- mayGetMarkB markName
    case mmark of
        Nothing -> fail $ "Mark " ++ markName ++ " not set"
        Just mark -> getMarkPointB mark >>= moveTo >> curLn

