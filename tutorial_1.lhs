\begin{align}

cabal repl
\end{align}


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Primitive as SDL
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
screenMiddle = fromIntegral screenWidth / 2
wallWidth = screenWidth `div` rayCount
wallHeight = 64

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

data Intersection = Intersection (V2 Float)

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
drawScreen = do
  (GameState w pdir ppos) <- get

      -----
  let rayAnglePairs = rayHeads rayCount pdir
      rays = fmap fst rayAnglePairs :: [V2 Float]
      angles = fmap snd rayAnglePairs :: [Float]

      paths = shootRay (fromIntegral $ worldSize w) ppos <$> rays
      wallPoints = walkRaysForWalls w ppos paths
      ----

  renderer <- asks cRenderer
  let test = (drawWall renderer ppos w) <$> zip3 wallPoints [0..] angles
  return ()


drawWall :: SDL.Renderer -> V2 Float -> WorldTiles -> ((Maybe Intersection), Float, Float) -> IO ()
drawWall _ _ _ (Nothing, _, _) = return ()
drawWall r p w (Just (Intersection intpos@(V2 x y)), rayIndex, rayAngle) = do
  let distanceToSlice = norm $ intpos - p
      projectedWallHeight = wallHeight / distanceToSlice
      wallTop     = screenMiddle - projectedWallHeight
      wallBottom  = screenMiddle + projectedWallHeight
      wallLeft    = rayIndex * fromIntegral wallWidth
      wallRight   = (rayIndex + 1) * fromIntegral wallWidth

      filledTileColor = SDL.V4 51 51 102 maxBound

  SDL.fillRectangle r (fmap floor $ V2 wallLeft wallTop) (fmap floor $ V2 wallRight wallBottom) filledTileColor

boxMap :: Int -> WorldTiles
boxMap n = WorldTiles tiles n
  where
    top = replicate n FullWall
    middle = [FullWall] ++ replicate (n - 2) EmptyWall ++ [FullWall]
    tiles = listArray (0,((n*n) - 1)) $ top ++ middle ++ top

accessMap :: WorldTiles -> V2 Int -> Wall
accessMap w (V2 x y) = tiles w ! ((x * worldSize w) + y)

--TODO raycasting stuff
rayHeads = undefined
shootRay = undefined
walkRaysForWalls = undefined

\end{code}
