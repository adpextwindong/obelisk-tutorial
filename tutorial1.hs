{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Primitive as SDL
import Linear
import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Foreign.C.Types
import Control.Lens
import Control.Concurrent
import Data.Bifunctor
import Data.Maybe

import Obelisk.Math.Vector
import Obelisk.Math.Homogenous
import qualified Debug.Trace as Debug

initVars = GameState {
  world = boxMap
  ,playerpos = V2 8.0 8.0
  ,playerdir = normalize $ V2 1 1
}


data Config = Config {
                cWindow :: SDL.Window
               ,cRenderer :: SDL.Renderer
               ,cSurface :: SDL.Surface
              }


data GameState = GameState {
                    world :: WorldTiles
                   ,playerpos :: V2 Float
                   ,playerdir :: V2 Float
                 }

data Wall = FullWall | EmptyWall
  deriving Show

data WorldTiles = WorldTiles {
                    tiles :: Array Int Wall
                  } deriving Show

data Intersection = Intersection (V2 Float) (V2 Int)

singleMap :: WorldTiles
singleMap = WorldTiles tiles
  where
    n = worldSize
    tiles = listArray (0,(n*n)-1) $ FullWall : replicate ((n * n) - 1) EmptyWall

emptyMap = let n = (worldSize * worldSize) in WorldTiles $ listArray (0,n) $ replicate n EmptyWall

boxMap :: WorldTiles
boxMap = WorldTiles tiles
  where
    n = worldSize
    top = replicate n FullWall
    middle = [FullWall] ++ replicate (n - 2) EmptyWall ++ [FullWall]
    tiles = listArray (0,(n*n) - 1) $ concat $ [top] ++ replicate (n - 2) middle ++ [top]

accessMap :: WorldTiles -> V2 Int -> Wall
accessMap w (V2 x y) = tiles w ! ((x * fromIntegral worldSize) + y)

--
-- Demo Constants
--
rayCount = 320
(screenWidth, screenHeight) = (640,480)
screenMiddle = fromIntegral screenHeight / 2
wallWidth = screenWidth `div` rayCount
wallHeight = 64
worldSize = 10

main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Raycaster" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  screenRenderer <- SDL.createSoftwareRenderer screenSurface

  let cfg = Config {
    cWindow = window,
    cRenderer = screenRenderer,
    cSurface = screenSurface
  }

  evalStateT (runReaderT renderLoop cfg) initVars

  SDL.freeSurface screenSurface
  SDL.destroyWindow window
  SDL.quit

renderLoop :: ReaderT Config (StateT GameState IO) ()
renderLoop = do
  let backgroundColor = SDL.V4 34 34 34 255

  events <- SDL.pollEvents
  --TODO mouseLookAt

  let quitSignal = SDL.KeycodeEscape `elem` [ k | x@(SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ k _)))) <- events]

  SDL.clear =<< asks cRenderer
  screenSurface <- asks cSurface
  SDL.surfaceFillRect screenSurface Nothing backgroundColor

  --drawDebug
  drawScreen

  SDL.updateWindowSurface =<< asks cWindow
  liftIO $ threadDelay 6000

  unless quitSignal renderLoop

--
-- Debug UI
--

drawDebug  :: ReaderT Config (StateT GameState IO) ()
drawDebug = do
  (GameState w@(WorldTiles tiles) ppos pdir) <- get
  rend <- asks cRenderer

  --Affine Transformation Utility functions
  let gtp = mapAft worldToPD
      btp = bimap gtp gtp

  --Grid Lines
  let verticalLines = btp <$> [(V2 x 0, V2 x (fromIntegral worldSize)) | x <- [0.. fromIntegral worldSize]]
      horizontalLines = btp <$> [(V2 0 y, V2 (fromIntegral worldSize) y) | y <- [0..(fromIntegral worldSize)]]

  --Tiles
  let inds = [(x,y) | x <- [0..worldSize - 1], y <- [0..worldSize - 1]]
      quads = [(V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) |
                  x <- fmap fromIntegral [0.. worldSize - 1], y <- fmap fromIntegral [0.. worldSize - 1]]

  sequence_ $ zip inds quads <&> (\(ind@(x,y), (vA,vB,vC,vD)) -> do
    let sampleColor = wallTypeToColor $ accessMap w (V2 x y)
    SDL.fillTriangle rend (gtp vA) (gtp vB) (gtp vC) sampleColor
    SDL.fillTriangle rend (gtp vB) (gtp vC) (gtp vD) sampleColor)

  sequence_ $ (\(a, b) -> SDL.line rend a b white) <$> verticalLines
  sequence_ $ (\(a, b) -> SDL.line rend a b white) <$> horizontalLines

  --Player Arrow
  let playerT = translateT (ppos ^._x) (ppos ^._y)
      arrowT = worldToPD !*! translateT (pdir ^._x) (pdir ^._y) !*! playerT !*! rotationT (vectorAngle pdir)
      dirLen = norm pdir
      arrowLength = 0.25
      arrowWidth = 0.06
      arrowHeadDisplacementT = translateT (1.05 * dirLen - arrowLength) 0.0
      red = SDL.V4 255 0 0 255

      arrowBase = ppos
      arrowHead = ppos + normalize pdir

  SDL.line rend (mapAft worldToPD arrowBase) (mapAft worldToPD arrowHead) red
  SDL.fillTriangle rend (mapAft arrowT (V2 0.0 (-arrowWidth)))
                        (mapAft arrowT (V2 arrowLength 0.0))
                        (mapAft arrowT (V2 0.0 arrowWidth))
                        (SDL.V4 255 51 51 255)


worldToPD = translateToPDCenter !*! zoomFactor !*! centerToLocalOrigin
  where
    delta = fromIntegral worldSize / 2
    centerToLocalOrigin = translateT (-delta) (-delta)
    zoomFactor = zoomT $ fromIntegral screenHeight / fromIntegral worldSize * 0.95
    translateToPDCenter = translateT (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)

wallTypeToColor FullWall = filledTileColor
wallTypeToColor EmptyWall = SDL.V4 34 34 34 maxBound
white = SDL.V4 255 255 255 255
filledTileColor = SDL.V4 51 51 102 maxBound
yellow :: SDL.Color
yellow = SDL.V4 255 255 0 maxBound

--
-- Renderer
--
drawScreen :: ReaderT Config (StateT GameState IO) ()
drawScreen = do
  (GameState w ppos pdir) <- get

  let rayAnglePairs = rayHeads (fromIntegral rayCount) pdir
      rays = fmap fst rayAnglePairs :: [V2 Float]
      angles = fmap snd rayAnglePairs :: [Float]
      paths = shootRay ppos <$> rays
      wallPoints = walkRayForWall w ppos <$> paths

  renderer <- asks cRenderer
  liftIO . sequence_ $ drawWall renderer ppos w <$> zip3 wallPoints [0..] angles

  -- Debug Wall Point circles
  --liftIO . sequence_ $ catMaybes wallPoints <&> (\(Intersection pos _) -> SDL.circle renderer (mapAft (worldToPD (worldSize w)) pos) 1 yellow)

drawWall :: SDL.Renderer -> V2 Float -> WorldTiles -> (Maybe Intersection, Float, Float) -> IO ()
drawWall _ _ _ (Nothing, _, _) = return ()
drawWall r p w (Just (Intersection intpos@(V2 x y) _), rayIndex, rayAngle) = do
  let distanceToSlice = norm $ intpos - p         -- Fish Eye --rayAngle * distance p intpos -- Permadi Fix
      projectedWallHeight = wallHeight / distanceToSlice
      wallTop     = screenMiddle - projectedWallHeight
      wallBottom  = screenMiddle + projectedWallHeight
      wallLeft    = rayIndex * fromIntegral wallWidth
      wallRight   = (rayIndex + 1) * fromIntegral wallWidth

  SDL.fillRectangle r (floor <$> V2 wallLeft wallTop) (floor <$> V2 wallRight wallBottom) filledTileColor

rayHeads :: Int -> V2 Float -> [(V2 Float, Float)]
rayHeads rayCount playerdir = fmap ray cameraPlaneSweep
  where
   cameraPlaneSweep = [2.0 * (x / fromIntegral rayCount) - 1.0 | x <- [0 .. fromIntegral rayCount - 1]]
   cameraPlane = normalize $ playerdir *! rotation2 (pi / 2.0)
   angleBetween v u = dot u v / (norm u * norm v)
   ray screenDelta =
    let ray = normalize $ playerdir - (cameraPlane ^* screenDelta) in (ray, angleBetween ray playerdir)

shootRay :: V2 Float -> V2 Float -> [Intersection]
shootRay playerpos direction = mergeIntersections playerpos vints hints
  where
    vints = if direction ^._x == 0.0
            then [] -- No vertical intersections if literally looking along x axis
            else boundedHorizontal $ xRayGridIntersections playerpos direction

    hints = boundedVertical $ yRayGridIntersections playerpos direction

boundedHorizontal :: [V2 Float] -> [V2 Float]
boundedHorizontal = takeWhile (\(V2 _ y) -> y > 0 && y < fromIntegral worldSize)

boundedVertical :: [V2 Float] -> [V2 Float]
boundedVertical = takeWhile (\(V2 x _) -> x > 0 && x < fromIntegral worldSize)

xRayGridIntersections :: V2 Float -> V2 Float -> [V2 Float]
xRayGridIntersections p nr = (p +) . (nr ^*) <$> stepScales
  where
    firstStep = abs $ deltaFirst (p ^._x) (nr ^._x)
    stepScales = [(firstStep + x + epsilon) / abs (nr ^._x) | x <- take (upperBound (p^._x) (nr ^._x)) [0.0 ..]]

yRayGridIntersections :: V2 Float -> V2 Float -> [V2 Float]
yRayGridIntersections p nr = (p +) . (nr ^*) <$> stepScales
  where
    firstStep = abs $ deltaFirst (p ^._y) (nr ^._y)
    stepScales = [(firstStep + y + epsilon) / abs (nr ^._y) | y <- take (upperBound (p^._y) (nr ^._y)) [0.0 ..]]

epsilon :: Float
epsilon = 0.00001

deltaFirst :: Float -> Float -> Float
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral ((floor px) + 1) - px

upperBound :: Float -> Float -> Int
upperBound axisPosition axisRay = if axisRay > 0
                                  then floor $ fromIntegral worldSize - axisPosition
                                  else floor axisPosition

mergeIntersections :: V2 Float -> [V2 Float] -> [V2 Float] -> [Intersection]
mergeIntersections playerpos v@(x:xs) h@(y:ys) = if qd playerpos x < qd playerpos y
                                                 then vX : mergeIntersections playerpos xs h
                                                 else hY : mergeIntersections playerpos v ys
  where vX = Intersection x $ fmap truncate x
        hY = Intersection y $ fmap truncate y

mergeIntersections _ [] ys = (\y -> Intersection y $ fmap truncate y) <$> ys
mergeIntersections _ xs [] = (\x -> Intersection x $ fmap truncate x) <$> xs

walkRayForWall :: WorldTiles -> V2 Float -> [Intersection] -> Maybe Intersection
walkRayForWall _ _ [] = Nothing
walkRayForWall w p (i@(Intersection _ checkInds) :path) =
  case accessMap w checkInds of
    FullWall -> Just i
    _ -> walkRayForWall w p path
