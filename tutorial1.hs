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

data Config = Config {
                cWindow :: SDL.Window
               ,cRenderer :: SDL.Renderer
               ,cSurface :: SDL.Surface
              }

rayCount = 320
(screenWidth, screenHeight) = (640,480)
screenMiddle = fromIntegral screenHeight / 2
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

data Intersection = Intersection (V2 Float) (V2 Int)

main = do
  SDL.initialize [SDL.InitVideo]
  let title = "Raycaster"

  window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  screenRenderer <- SDL.createSoftwareRenderer screenSurface

  let cfg = Config {
    cWindow = window,
    cRenderer = screenRenderer,
    cSurface = screenSurface
  }

  let initVars = GameState {
    world = boxMap 10
    ,playerpos = V2 5.0 5.0
    ,playerdir = normalize $ V2 (-0.5) (-0.5)
  }

  evalStateT (runReaderT renderLoop cfg) initVars

  SDL.freeSurface screenSurface
  SDL.destroyWindow window
  SDL.quit

renderLoop :: ReaderT Config (StateT GameState IO) ()
renderLoop = do
  let backgroundColor = SDL.V4 34 34 34 255

  events <- SDL.pollEvents
  --TODO updateInput and quit signal
  --TODO mouseLookAt
  let quitSignal = False

  SDL.clear =<< asks cRenderer
  screenSurface <- asks cSurface
  SDL.surfaceFillRect screenSurface Nothing backgroundColor

  --drawScreen
  drawDebug

  SDL.updateWindowSurface =<< asks cWindow
  liftIO $ threadDelay 6000

  unless quitSignal renderLoop


drawDebug  :: ReaderT Config (StateT GameState IO) ()
drawDebug = do
  (GameState w@(WorldTiles tiles ws) pdir ppos) <- get
  rend <- asks cRenderer

  --Affine Transformation Utility functions
  let gtp = mapAft $ worldToPD ws
      btp = bimap gtp gtp

  --Grid Lines
  let verticalLines = btp <$> [((V2 x 0),(V2 x (fromIntegral ws))) | x <- [0.. fromIntegral ws]] :: [(V2 CInt, V2 CInt)]
      horizontalLines = btp <$> [((V2 0 y),(V2 (fromIntegral ws) y)) | y <- [0..(fromIntegral ws)]]

  --Tiles
  let inds = [(x,y) | x <- [0..ws - 1], y <- [0..ws - 1]]
      quads = [(V2 x y, V2 (x+1) y, V2 x (y+1), V2 (x+1) (y+1)) |
                  x <- fmap fromIntegral [0.. ws - 1], y <- fmap fromIntegral [0.. ws - 1]]

  sequence_ $ zip inds quads <&> (\(ind@(x,y), (vA,vB,vC,vD)) -> do
    let sampleColor = wallTypeToColor $ accessMap w (V2 x y)
    SDL.fillTriangle rend (gtp vA) (gtp vB) (gtp vC) sampleColor
    SDL.fillTriangle rend (gtp vB) (gtp vC) (gtp vD) sampleColor)

  sequence_ $ uncurry (\a b -> SDL.line rend a b white) <$> verticalLines
  sequence_ $ uncurry (\a b -> SDL.line rend a b white) <$> horizontalLines

worldToPD ws = translateToPDCenter !*! zoomFactor !*! centerToLocalOrigin
  where
    delta = fromIntegral ws / 2
    centerToLocalOrigin = translateT (-delta) (-delta)
    zoomFactor = zoomT $ fromIntegral screenHeight / fromIntegral ws * 0.95
    translateToPDCenter = translateT (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)

mapAft :: V3 (V3 Float) -> V2 Float -> V2 CInt
mapAft t = dC . (fmap floor . (t !*)) . hC

hC :: (Num a) => V2 a -> V3 a
hC (V2 x y) = V3 x y 1

dC :: (Num a) => V3 a -> V2 a
dC (V3 x y _) = V2 x y

translateT :: (Num a) => a -> a -> V3 (V3 a)
translateT x y = V3 (V3 1 0 x)
                   (V3 0 1 y)
                   (V3 0 0 1)

zoomT :: (Num a) => a -> V3 (V3 a)
zoomT scale = V3 (V3 scale 0 0)
                (V3 0 scale 0)
                (V3 0 0 1)


wallTypeToColor FullWall = filledTileColor
wallTypeToColor EmptyWall = SDL.V4 34 34 34 maxBound
white = SDL.V4 255 255 255 255
filledTileColor = SDL.V4 51 51 102 maxBound

drawScreen :: ReaderT Config (StateT GameState IO) ()
drawScreen = do
  (GameState w pdir ppos) <- get

  let rayAnglePairs = rayHeads rayCount pdir
      rays = fmap fst rayAnglePairs :: [V2 Float]
      angles = fmap snd rayAnglePairs :: [Float]

      paths = shootRay (fromIntegral $ worldSize w) ppos <$> rays
      wallPoints = walkRayForWall w ppos <$> paths

  renderer <- asks cRenderer
  liftIO . sequence_ $ (drawWall renderer ppos w) <$> zip3 wallPoints [0..] angles

drawWall :: SDL.Renderer -> V2 Float -> WorldTiles -> ((Maybe Intersection), Float, Float) -> IO ()
drawWall _ _ _ (Nothing, _, _) = return ()
drawWall r p w (Just (Intersection intpos@(V2 x y) _), rayIndex, rayAngle) = do
  let distanceToSlice = norm $ intpos - p
      projectedWallHeight = wallHeight / distanceToSlice
      wallTop     = screenMiddle - projectedWallHeight
      wallBottom  = screenMiddle + projectedWallHeight
      wallLeft    = rayIndex * fromIntegral wallWidth
      wallRight   = (rayIndex + 1) * fromIntegral wallWidth


  SDL.fillRectangle r (fmap floor $ V2 wallLeft wallTop) (fmap floor $ V2 wallRight wallBottom) filledTileColor

singleMap :: Int -> WorldTiles
singleMap n = WorldTiles tiles n
  where
    tiles = listArray (0,((n*n)-1)) $ [FullWall] ++ replicate ((n * n) - 1) EmptyWall

boxMap :: Int -> WorldTiles
boxMap n = WorldTiles tiles n
  where
    top = replicate n FullWall
    middle = [FullWall] ++ replicate (n - 2) EmptyWall ++ [FullWall]
    tiles = listArray (0,((n*n) - 1)) $ concat $ [top] ++ (replicate (n - 2) middle) ++ [top]

accessMap :: WorldTiles -> V2 Int -> Wall
accessMap w (V2 x y) = tiles w ! ((x * fromIntegral (worldSize w)) + y)

rayHeads playerpos playerdir = fmap ray cameraPlaneSweep
  where
   cameraPlaneSweep = [2.0 * (x / fromIntegral screenWidth) - 1.0 | x <- [0 .. fromIntegral screenWidth - 1]]
   cameraPlane = normalize $ playerdir *! rotation2 (pi / 2.0)
   cosThetaBetween v u = dot u v / (norm u * norm v)
   ray screenDelta =
    let ray = normalize $ playerdir - cameraPlane ^* screenDelta in (ray, cosThetaBetween ray playerdir)

shootRay :: Int -> V2 Float -> V2 Float -> [Intersection]
shootRay ws playerpos direction = mergeIntersections playerpos vints hints
  where
    stepsX = baseStepsBounded ws (playerpos ^._x) (direction ^._x)
    stepsY = baseStepsBounded ws (playerpos ^._y) (direction ^._y)

    vints = if direction ^._x == 0.0
            then [] -- No vertical intersections if literally looking along x axis
            else boundedHorizontal ws $ xRayGridIntersections playerpos direction stepsX

    hints = boundedVertical ws $ yRayGridIntersections playerpos direction stepsY

baseSteps :: [Float]
baseSteps = [0.0 ..]

upperBound :: Int -> Float -> Float -> Int
upperBound ws axisPosition axisRay = if axisRay > 0
                                     then floor $ fromIntegral ws - axisPosition
                                     else floor axisPosition

baseStepsBounded ws axisPosition axisRay = take (upperBound ws axisPosition axisRay) baseSteps

boundedHorizontal ws = takeWhile (\(V2 _ y) -> y > 0 && y < fromIntegral ws)

boundedVertical ws = takeWhile (\(V2 x _) -> x > 0 && x < fromIntegral ws)

epsilon = 0.00001

xRayGridIntersections p nr bss = (p +) . (nr ^*) <$> stepScales
  where
    firstStep = abs $ deltaFirst (p ^._x) (nr ^._x)
    stepScales = [(firstStep + x + epsilon) / abs (nr ^._x) | x <- bss]

yRayGridIntersections p nr bss = (p +) . (nr ^*) <$> stepScales
  where
    firstStep = abs $ deltaFirst (p ^._y) (nr ^._y)
    stepScales = [(firstStep + y + epsilon) / abs (nr ^._y) | y <- bss]

deltaFirst :: Float -> Float -> Float
deltaFirst px vx = if vx < 0
                   then fromIntegral (floor px) - px
                   else fromIntegral (ceiling px) - px


mergeIntersections playerpos v@(x:xs) h@(y:ys) = if qd playerpos x < qd playerpos y
                                                 then vX : mergeIntersections playerpos xs h
                                                 else hY : mergeIntersections playerpos v ys
  where vX = Intersection x $ fmap truncate x
        hY = Intersection y $ fmap truncate y

mergeIntersections _ [] ys = (\y -> Intersection y $ fmap truncate y) <$> ys
mergeIntersections _ xs [] = (\x -> Intersection x $ fmap truncate x) <$> xs

walkRayForWall :: WorldTiles -> V2 Float -> [Intersection] -> Maybe Intersection
walkRayForWall _ _ [] = Nothing
walkRayForWall w p (i@(Intersection _ checkInds) :path) = case accessMap w checkInds of
                                         FullWall -> Just i
                                         _ -> walkRayForWall w p path

rotation2 :: Float -> V2 (V2 Float)
rotation2 theta = V2 (V2 (cos theta) (-sin theta))
                     (V2 (sin theta) (cos theta))
