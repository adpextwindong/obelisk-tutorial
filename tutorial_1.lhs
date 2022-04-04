\begin{align}

cabal repl
\end{align}


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
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

rayCount = 320
(screenWidth, screenHeight) = (640,480)

data GameState = GameState {
                    world :: WorldTiles
                   ,playerpos :: V2 Float
                   ,playerdir :: V2 Float
                 }

data Wall = FullWall | EmptyWall

data WorldTiles = WorldTiles {
                    tiles :: Array Int Wall
                   ,worldSize :: Int
                  }

main = do
  SDL.initialize [SDL.InitVideo]
  let title = "Raycaster"

  window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  SDL.updateWindowSurface window

  screenRenderer <- SDL.createSoftwareRenderer screenSurface

  let cfg = Config {
    cWindow = window,
    cRenderer = screenRenderer,
    cSurface = screenSurface
  }

  --TODO
  let initVars = GameState {
    world = boxMap 10
    ,playerpos = undefined
    ,playerdir = undefined
  }

  evalStateT (runReaderT renderLoop cfg) initVars

  SDL.freeSurface screenSurface
  SDL.destroyWindow window
  SDL.quit

renderLoop :: ReaderT Config (StateT GameState IO) ()
renderLoop = do
  let backgroundColor = SDL.V4 34 34 34 255

  --TODO updateInput and quit signal
  let quitSignal = False

  SDL.clear =<< asks cRenderer
  screenSurface <- asks cSurface
  SDL.surfaceFillRect screenSurface Nothing backgroundColor

  --TODO
  drawScreen

  unless quitSignal renderLoop

  undefined

\end{code}

\begin{code}

--TODO https://github.com/adpextwindong/obelisk/blob/main/src/Obelisk/Effect/Renderer.hs#L190
drawScreen :: ReaderT Config (StateT GameState IO) ()
drawScreen = undefined

boxMap :: Int -> WorldTiles
boxMap n = WorldTiles tiles n
  where
    top = replicate n FullWall
    middle = [FullWall] ++ replicate (n - 2) EmptyWall ++ [FullWall]
    tiles = listArray (0,((n*n) - 1)) $ top ++ middle ++ top

accessMap :: WorldTiles -> V2 Int -> Wall
accessMap w (V2 x y) = tiles w ! ((x * worldSize w) + y)

\end{code}
