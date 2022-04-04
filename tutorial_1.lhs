\begin{align}

cabal repl
\end{align}


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import qualified SDL
import Linear
import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Foreign.C.Types

data Config = Config {
                cWindow :: SDL.Window
               ,cRenderer :: SDL.Renderer
               ,cSurface :: SDL.Surface
              }

(initialScreenWidth, initialScreenHeight) = (640,480)

data GameState = GameState {
                    tiles :: Array Int Wall
                 }

data Wall = FullWall | EmptyWall

main = do
  SDL.initialize [SDL.InitVideo]
  let title = "Raycaster"

  window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 initialScreenWidth initialScreenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  SDL.updateWindowSurface window

  screenRenderer <- SDL.createSoftwareRenderer screenSurface

  let cfg = Config {
    cWindow = window,
    cRenderer = screenRenderer,
    cSurface = screenSurface
  }

  let initVars = GameState {
    tiles = undefined
  }

  evalStateT (runReaderT renderLoop cfg) initVars

  SDL.freeSurface screenSurface
  SDL.destroyWindow window
  SDL.quit

renderLoop :: ReaderT Config (StateT GameState IO) ()
renderLoop = do
  let backgroundColor = SDL.V4 34 34 34 255

  --TODO updateInput and quit signal

  SDL.clear =<< asks cRenderer
  screenSurface <- asks cSurface
  SDL.surfaceFillRect screenSurface Nothing backgroundColor

  --TODO

  undefined

\end{code}
