module Diagrams.Segment.SpiroSplineSpec (spec) where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Diagrams.Prelude (
  Closed,
  Segment,
  Trail,
  V2 (V2),
  bezier3,
  closeTrail,
  isLoop,
  mapSegmentVectors,
  p2,
  straight,
  trailFromSegments,
  trailSegments,
 )
import Test.Hspec (Spec, describe, it, shouldReturn)

import Diagrams.Segment.Internal.TypesSpec (roundTo)
import Diagrams.Segment.SpiroSpline (
  SpiroPoint (..),
  SpiroSpline (..),
  spiroSplineToTrail,
 )

spec :: Spec
spec = do
  describe "spiroSplineToTrail" $ do
    it "generates expected output for circle"
      $ splineToTrail circle `shouldReturn` excpectedCircle
    it "generates expected output for openWithPoints"
      $ splineToTrail openWithPoints `shouldReturn` expectedOpenWithPoints
    it "generates expected output for path5"
      $ splineToTrail path5 `shouldReturn` expectedPath5

splineToTrail :: SpiroSpline (Double, Double) -> IO (Trail V2 Double)
splineToTrail =
  fmap (mapTrail (fmap (roundTo 3))) . spiroSplineToTrail . fmap p2

mapTrail :: (V2 Double -> V2 Double) -> Trail V2 Double -> Trail V2 Double
mapTrail f t =
  close . trailFromSegments . map (mapSegmentVectors f) $ trailSegments t
  where
    close =
      if isLoop t
        then closeTrail
        else id

{- |
Unit test example from
<https://github.com/fontforge/libspiro/blob/20240903/README.md>
-}
circle :: (Num x, Num y) => SpiroSpline (x, y)
circle =
  Closed
    $ SpiroCurveG4 (-100, 0)
      :| [ SpiroCurveG4 (0, 100)
         , SpiroCurveG4 (100, 0)
         , SpiroCurveG4 (0, -100)
         ]

bezier :: (n, n) -> (n, n) -> (n, n) -> Segment Closed V2 n
bezier point1 point2 point3 =
  bezier3 (uncurry V2 point1) (uncurry V2 point2) (uncurry V2 point3)

excpectedCircle :: Trail V2 Double
excpectedCircle =
  [ bezier (0.0, 26.18) (10.777, 52.199) (29.289, 70.711)
  , bezier (18.512, 18.512) (44.531, 29.289) (70.711, 29.289)
  , bezier (26.18, 0.0) (52.199, -10.777) (70.711, -29.289)
  , bezier (18.512, -18.512) (29.289, -44.531) (29.289, -70.711)
  , bezier (0.0, -26.18) (-10.777, -52.199) (-29.289, -70.711)
  , bezier (-18.512, -18.512) (-44.531, -29.289) (-70.711, -29.289)
  , bezier (-26.18, 0.0) (-52.199, 10.777) (-70.711, 29.289)
  , bezier (-18.512, 18.512) (-29.289, 44.531) (-29.289, 70.711)
  , straight (V2 0.0 0.0)
  ]
    & closeTrail . trailFromSegments

openWithPoints :: (Num x, Num y) => SpiroSpline (x, y)
openWithPoints =
  Open
    (100, 100)
    (SpiroCurveG2 (200, 100) :| [SpiroCurveG2 (200, 200)])
    (100, 200)

expectedOpenWithPoints :: Trail V2 Double
expectedOpenWithPoints =
  [ bezier (13.09, -13.09) (31.488, -20.711) (50.0, -20.711)
  , bezier (18.512, 0.0) (36.91, 7.621) (50.0, 20.711)
  , bezier (13.09, 13.09) (20.711, 31.488) (20.711, 50.0)
  , bezier (0.0, 18.512) (-7.621, 36.91) (-20.711, 50.0)
  , bezier (-13.09, 13.09) (-31.488, 20.711) (-50.0, 20.711)
  , bezier (-18.512, 0.0) (-36.91, -7.621) (-50.0, -20.711)
  ]
    & trailFromSegments

{- |
Unit test example from
<https://github.com/fontforge/libspiro/blob/20240903/tests/call-test.c>
-}
path5 :: (Num x, Num y) => SpiroSpline (x, y)
path5 =
  Open
    { startPoint = (0, 0)
    , spiroPoints =
        SpiroCurveG4 (100, 100)
          :| [ SpiroLeftConstraint (200, 200)
             , SpiroRightConstraint (300, 200)
             , SpiroCurveG4 (400, 150)
             , SpiroLeftConstraint (300, 100)
             , SpiroRightConstraint (200, 100)
             , SpiroCurveG4 (150, 50)
             , SpiroLeftConstraint (100, 0)
             , SpiroRightConstraint (0, -100)
             , SpiroCurveG4 (-50, -200)
             ]
    , endPoint = (-80, -250)
    }

expectedPath5 :: Trail V2 Double
expectedPath5 =
  [ bezier (25.789, -0.383) (51.303, 11.233) (68.669, 30.303)
  , bezier (17.366, 19.069) (25.904, 44.483) (31.331, 69.697)
  , bezier (2.8, 13.012) (5.111, 26.162) (9.086, 38.864)
  , bezier (3.975, 12.702) (9.787, 24.997) (18.48, 35.075)
  , bezier (8.693, 10.078) (20.235, 17.644) (32.924, 21.662)
  , bezier (12.688, 4.018) (26.201, 4.399) (39.51, 4.399)
  , straight (V2 100.0 0.0)
  , bezier (20.922, 0.0) (42.348, 1.061) (62.295, -5.249)
  , bezier (9.974, -3.155) (19.436, -8.441) (26.431, -16.219)
  , bezier (6.995, -7.778) (11.274, -18.071) (11.274, -28.532)
  , bezier (0.0, -10.461) (-4.279, -20.754) (-11.274, -28.532)
  , bezier (-6.995, -7.778) (-16.457, -13.064) (-26.431, -16.219)
  , bezier (-19.947, -6.31) (-41.374, -5.249) (-62.295, -5.249)
  , straight (V2 (-100.0) 0.0)
  , bezier (-6.491, 0.0) (-13.116, -0.28) (-19.17, -2.622)
  , bezier (-6.054, -2.342) (-11.282, -6.613) (-15.085, -11.873)
  , bezier (-3.803, -5.26) (-6.253, -11.361) (-8.462, -17.464)
  , bezier (-2.209, -6.103) (-4.305, -12.274) (-7.284, -18.041)
  , bezier (-5.443, -10.54) (-14.066, -19.066) (-23.275, -26.544)
  , bezier (-9.209, -7.478) (-18.337, -15.068) (-26.725, -23.456)
  , straight (V2 (-100.0) (-100.0))
  , bezier (-13.399, -13.399) (-25.576, -28.346) (-32.928, -45.811)
  , bezier (-7.352, -17.464) (-10.856, -36.289) (-17.072, -54.189)
  , bezier (-6.43, -18.518) (-16.278, -36.001) (-30.0, -50.0)
  ]
    & trailFromSegments
