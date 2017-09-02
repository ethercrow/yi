{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Yi.Frontend.SDL
    ( start
    ) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Maybe
import GHC.Conc (labelThread)
import System.Exit

import SDL
import SDL.Font

import Yi.Config (UIBoot)
import Yi.Editor
import Yi.Event
import qualified Yi.UI.Common as Common
import qualified Yi.UI.SimpleLayout as SL

data FrontendState = FrontendState
    { fsEndMain :: MVar ExitCode
    , fsDirty :: MVar ()
    , fsWindow :: SDL.Window
    , fsFont :: SDL.Font.Font
    , fsSubmitEvents :: [Yi.Event.Event] -> IO ()
    , fsEditorRef :: IORef Editor
    }

start :: UIBoot
start config submitEvents submitActions editor = do

    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    window <- SDL.createWindow "yi" SDL.defaultWindow
    font <- SDL.Font.load "/Users/ethercrow/Downloads/FiraCode-Regular.ttf" 70
    SDL.showWindow window

    endMain <- newEmptyMVar
    dirty <- newEmptyMVar
    editorRef <- newIORef editor
    let fs = FrontendState endMain dirty window font submitEvents editorRef

    return $! Common.dummyUI
        { Common.main = main fs
        , Common.end = end fs
        , Common.refresh = requestRefresh fs
        , Common.userForceRefresh = userForceRefresh fs
        , Common.layout = layout fs
        }

requestRefresh :: FrontendState -> Editor -> IO ()
requestRefresh fs e = do
    writeIORef (fsEditorRef fs) e
    void $ tryPutMVar (fsDirty fs) ()

main :: FrontendState -> IO ()
main fs@(FrontendState endMain dirty window font submitEvents editorRef) = do
    tid <- myThreadId
    labelThread tid "SDLMain"

    let loop = do
            SDL.pollEvents >>= \case
                [] -> pure ()
                evs -> submitEvents (fmap convertEvent evs)
            tryTakeMVar endMain >>= \case
                Nothing ->
                    readIORef editorRef >>= refresh fs
                    threadDelay 16000 -- TODO: subtract work time on this iteration
                    loop
                Just exitCode -> pure exitCode

    exitCode <- loop

    SDL.destroyWindow window
    SDL.Font.quit
    SDL.quit

    exitWith exitCode

layout :: FrontendState -> Editor -> IO Editor
layout fs e = error "layout not implemented"

end :: FrontendState -> Maybe ExitCode -> IO ()
end fs mExit = putMVar (fsEndMain fs) (fromMaybe ExitSuccess mExit)

userForceRefresh fs = error "userForceRefresh not implemented"

refresh fs = error "refresh not implemented"

convertEvent e = error "convertEvent not implemented"