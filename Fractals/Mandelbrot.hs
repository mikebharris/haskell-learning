
import Data.List
import Data.Complex
-- import qualified Graphics.Image as G

----------------------
-- a Mandelbrot Set --
----------------------
-- for every value of the complex number C on the complexplane 
-- starting with Z = (0 + 0i)
-- compute the iteration Z -> Z^2 + C
-- and if the result tends to infinity it is unstable and outside the set (Z > some threshold, say 10,000)
-- and if the result tends to zero it is stable and inside the set (maxiters exceeded after n iterations)

data AxisSection = AxisSection Double Double deriving Show

-- Example: putStrLn $ plottedMset 200 80 (AxisSection (-2.5) 0.8) (AxisSection (-1.25) 1.25)
plottedMset :: Double -> Double -> AxisSection -> AxisSection -> String
plottedMset xRes yRes xSection ySection = intercalate "" [ toString p | p <- mandelbrot xRes yRes xSection ySection] where
  toString (1.0, _, True) = "\n*"
  toString (1.0, _, False) = "\n."
  toString (_, _, True) = "*"
  toString _ = "."

-- graphicMset :: Double -> Double -> String
-- graphicMset xRes yRes imageFileName = G.writeImage imageFileName (msetImageGrid :: G.Image G.RPU G.Y Double) where
--   toColourGrid (_, _, True) = 0
--   toColourGrid (_, _, False) = 255
--   msetImageGrid = G.makeImageR G.RPU (round xRes :: Int, round yRes :: Int) (\(i, j) -> G.PixelY $ [ toColourGrid p | p <- mandelbrot xRes yRes] !! i * xRes + j :: Double)

-- mSetColourGrid :: Double -> Double -> [Int]
-- mSetColourGrid xRes yRes = [ toColourGrid p | p <- mandelbrot xRes yRes] where
--   toColourGrid (a, b, True) = 0
--   toColourGrid (a, b, False) = 255

-- Takes x resolution and y resolution, and returns a tuple for each point
-- i.e. (xCoord, yCoord, InOrOutOfSet)
-- The two dimensional graph represents the complex plane (x for real, y for imnaginary)
mandelbrot :: Double -> Double -> AxisSection -> AxisSection -> [(Double, Double, Bool)]
mandelbrot xRes yRes (AxisSection xMin xMax) (AxisSection yMin yMax) = [(x, y, mSetCompute (realPart x :+ imaginaryPart y)) | y <- [1..yRes], x <- [1..xRes]]
  where
    -- xMin = -2.5 -- leftmost
    -- xMax = 0.8 -- rightmost end of the area covered by the set (with a bit of padding)
    -- yMin = -1.25 -- bottom
    -- yMax = 1.25 -- top
    xStep = abs (xMax - xMin) / xRes -- the distance along the x axis covered by each pixel (i.e. how wide a pixel is)
    yStep = abs (yMax - yMin) / yRes -- ditto y axis
    realPart x = xMin + (xStep * x) -- the real component of the point on the complex plane coresponding to the point (x,y)
    imaginaryPart y = yMin + (yStep * y) -- the imaginary component of said point

mSetCompute :: Complex Double -> Bool
mSetCompute = mSetIterate iters z
  where
    z = 0 :+ 0
    iters = 0

-- For each point c on the complex plane, we do a certain number of iterations of z = z^2 + c
-- to see if z will tend towards infinity or not.
-- if z reaches the threshold then we assume it is on its way to infinity (and therefore not in the mandelbrot set)
-- Otherwise we assume it is not going towards infinity & is therefore in the set.
mSetIterate :: Int -> Complex Double -> Complex Double -> Bool
mSetIterate iters z c
  | (realPart z)^2 + (imagPart z)^2 > threshold = False -- the threshold has been reached indicating that z is tending towards infinity so c is not in the set
  | iters > maxIters = True -- it hasn't reached the threshold before max iterations
  | otherwise = mSetIterate (iters+1) newZ c -- move on to the next iteration
  where
    maxIters = 100
    threshold = 10000
    newZ = z^2 + c

