module Yi.Keymap.Vim2.StyledRegion
    ( StyledRegion(..)
    , normalizeRegion
    , transformCharactersInRegionB
    , transformCharactersInLineN
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Buffer
import Yi.Region

data StyledRegion = StyledRegion !RegionStyle !Region

normalizeRegion :: StyledRegion -> BufferM StyledRegion
normalizeRegion sr@(StyledRegion style reg) =
    -- from vim help:
    --
    -- 1. If the motion is exclusive and the end of the motion is in column 1, the
    --    end of the motion is moved to the end of the previous line and the motion
    --    becomes inclusive.  Example: "}" moves to the first line after a paragraph,
    --    but "d}" will not include that line.
    -- 						*exclusive-linewise*
    -- 2. If the motion is exclusive, the end of the motion is in column 1 and the
    --    start of the motion was at or before the first non-blank in the line, the
    --    motion becomes linewise.  Example: If a paragraph begins with some blanks
    --    and you do "d}" while standing on the first non-blank, all the lines of
    --    the paragraph are deleted, including the blanks.  If you do a put now, the
    --    deleted lines will be inserted below the cursor position.
    --
    -- TODO: case 2
    if style == Exclusive
    then do
        let end = regionEnd reg
        (_, endColumn) <- getLineAndColOfPoint end
        if endColumn == 0
        then return $ StyledRegion Inclusive $ reg { regionEnd = end -~ 2 }
        else return sr
    else return sr

transformCharactersInRegionB :: StyledRegion -> (Char -> Char) -> BufferM ()
transformCharactersInRegionB (StyledRegion Block reg) f = do
    subregions <- splitBlockRegionToContiguousSubRegionsB reg
    forM_ subregions $ \sr ->
        transformCharactersInRegionB (StyledRegion Exclusive sr) f
    case subregions of
        (sr:_) -> moveTo (regionStart sr)
        [] -> error "Should never happen"
transformCharactersInRegionB (StyledRegion style reg) f = do
    reg' <- convertRegionToStyleB reg style
    s <- readRegionB reg'
    replaceRegionB reg' (fmap f s)
    moveTo (regionStart reg')

transformCharactersInLineN :: Int -> (Char -> Char) -> BufferM ()
transformCharactersInLineN count action = do
    p0 <- pointB
    moveXorEol count
    p1 <- pointB
    let sreg = StyledRegion Exclusive $ mkRegion p0 p1
    transformCharactersInRegionB sreg action
    moveTo p1
