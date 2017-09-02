module Yi.Config.Default.SDL (configureSDL) where

import Lens.Micro.Platform ((.=))
import Yi.Frontend.SDL     (start)
import Yi.Config.Lens      (startFrontEndA)
import Yi.Config.Simple    (ConfigM)

configureSDL :: ConfigM ()
configureSDL = startFrontEndA .= start
