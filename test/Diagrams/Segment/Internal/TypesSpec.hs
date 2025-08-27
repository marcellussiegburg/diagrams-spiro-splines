module Diagrams.Segment.Internal.TypesSpec where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Diagrams.Prelude (p2, unp2)
import Test.Hspec (Spec, describe, it, shouldReturn)

import Diagrams.Segment.SpiroSpline.Internal.FFI (runSpiro)
import Diagrams.Segment.SpiroSpline.Internal.Types (
  SpiroPoint (..),
  SpiroSpline (..),
  SplineSegment (..),
  convertBezierSegment,
  fromSpiroSpline,
 )

spec :: Spec
spec =
  describe "underlying C library generates expected output for" $ do
    it "generates expected output for path0"
      $ splineToSegments path0 `shouldReturn` expectedPath0
    it "generates expected output for path6"
      $ splineToSegments path6 `shouldReturn` expectedPath6

splineToSegments
  :: SpiroSpline (Double, Double) -> IO [SplineSegment (Double, Double)]
splineToSegments =
  fmap toApproximateBezierSegment . runSpiro . fromSpiroSpline . fmap p2
  where
    toApproximateBezierSegment = map $ roundPoints . convertBezierSegment
    roundPoints = fmap (unp2 . fmap (roundTo 2))

roundTo :: Int -> Double -> Double
roundTo n x = fromInteger (round $ x * factor) / factor
  where
    factor = 10 ^ n

{- |
Unit test example from
<https://github.com/fontforge/libspiro/blob/20240903/tests/call-test21.c>
-}
path0 :: (Num x, Num y) => SpiroSpline (x, y)
path0 =
  [ SpiroCorner (305, 176)
  , SpiroCurveG2 (212, 142)
  , SpiroCurveG2 (159, 171)
  , SpiroCurveG2 (224, 237)
  , SpiroCurveG2 (347, 335)
  , SpiroCurveG2 (202, 467)
  , SpiroCorner (81, 429)
  , SpiroCorner (114, 368)
  , SpiroCurveG2 (201, 402)
  , SpiroCurveG2 (276, 369)
  , SpiroCurveG2 (218, 308)
  , SpiroCurveG2 (91, 211)
  , SpiroCurveG2 (124, 111)
  , SpiroCurveG2 (229, 82)
  ]
    & Closed
      . (SpiroCorner (334, 117) :|)

{- |
Expected result for 'path0' from
<https://github.com/fontforge/libspiro/blob/20240903/tests/call-test21.c>
-}
expectedPath0 :: [SplineSegment (Double, Double)]
expectedPath0 =
  [ MoveTo (334, 117)
  , Close (334, 117)
  , LineTo (334, 117) (305, 176)
  , Close (305, 176)
  , CubicBezier (305, 176) (279.449, 153.974) (245.733, 141.648) (212, 142)
  , Close (212, 142)
  , CubicBezier (212, 142) (201.226, 142.112) (190.267, 143.529) (180.511, 148.103)
  , CubicBezier
      (180.511, 148.103)
      (175.633, 150.39)
      (171.099, 153.461)
      (167.348, 157.327)
  , CubicBezier (167.348, 157.327) (163.596, 161.194) (160.641, 165.869) (159, 171)
  , Close (159, 171)
  , CubicBezier (159, 171) (156.364, 179.243) (157.251, 188.403) (160.654, 196.36)
  , CubicBezier
      (160.654, 196.36)
      (164.057, 204.316)
      (169.853, 211.112)
      (176.664, 216.45)
  , CubicBezier (176.664, 216.45) (190.287, 227.126) (207.444, 231.953) (224, 237)
  , Close (224, 237)
  , CubicBezier (224, 237) (250.322, 245.025) (276.649, 254.369) (299.324, 269.96)
  , CubicBezier (299.324, 269.96) (321.998, 285.551) (340.921, 308.162) (347, 335)
  , Close (347, 335)
  , CubicBezier (347, 335) (351.166, 353.392) (349.066, 373.009) (341.95, 390.473)
  , CubicBezier
      (341.95, 390.473)
      (334.833, 407.936)
      (322.824, 423.251)
      (308.134, 435.075)
  , CubicBezier (308.134, 435.075) (278.752, 458.724) (239.706, 467.893) (202, 467)
  , Close (202, 467)
  , CubicBezier (202, 467) (159.176, 465.986) (116.715, 452.651) (81, 429)
  , Close (81, 429)
  , LineTo (81, 429) (114, 368)
  , Close (114, 368)
  , CubicBezier (114, 368) (140.251, 385.152) (170.075, 396.808) (201, 402)
  , Close (201, 402)
  , CubicBezier (201, 402) (216.006, 404.519) (231.742, 405.476) (246.158, 400.607)
  , CubicBezier
      (246.158, 400.607)
      (253.366, 398.172)
      (260.137, 394.269)
      (265.488, 388.861)
  , CubicBezier (265.488, 388.861) (270.839, 383.452) (274.72, 376.5) (276, 369)
  , Close (276, 369)
  , CubicBezier (276, 369) (277.287, 361.46) (275.914, 353.582) (272.701, 346.641)
  , CubicBezier
      (272.701, 346.641)
      (269.488, 339.699)
      (264.498, 333.665)
      (258.672, 328.709)
  , CubicBezier (258.672, 328.709) (247.02, 318.796) (232.367, 313.255) (218, 308)
  , Close (218, 308)
  , CubicBezier (218, 308) (191.978, 298.481) (165.57, 289.162) (142.279, 274.152)
  , CubicBezier (142.279, 274.152) (118.988, 259.143) (98.727, 237.609) (91, 211)
  , Close (91, 211)
  , CubicBezier (91, 211) (85.821, 193.166) (86.581, 173.749) (92.587, 156.176)
  , CubicBezier (92.587, 156.176) (98.592, 138.603) (109.749, 122.908) (124, 111)
  , Close (124, 111)
  , CubicBezier (124, 111) (138.28, 99.068) (155.539, 90.934) (173.578, 86.363)
  , CubicBezier (173.578, 86.363) (191.617, 81.793) (210.435, 80.713) (229, 82)
  , Close (229, 82)
  , CubicBezier (229, 82) (266.199, 84.579) (302.694, 96.743) (334, 117)
  ]

{- |
Unit test example from
<https://github.com/fontforge/libspiro/blob/20240903/tests/call-test21.c>
-}
path6 :: (Num x, Num y) => SpiroSpline (x, y)
path6 =
  Open
    { startPoint = (0, 0)
    , spiroPoints =
        SpiroCurveG4 (100, 100)
          :| [ SpiroAnchor (200, 200) (300, 200)
             , SpiroCurveG4 (300, 150)
             , SpiroAnchor (200, 100) (150, 100)
             , SpiroCurveG4 (150, 50)
             , SpiroAnchor (100, 0) (0, -100)
             , SpiroCurveG4 (50, -100)
             ]
    , endPoint = (20, -150)
    }

expectedPath6 :: [SplineSegment (Double, Double)]
expectedPath6 =
  [ MoveTo (0.0, 0.0)
  , Close (0.0, 0.0)
  , CubicBezier (0.0, 0.0) (25.789, -0.383) (51.303, 11.233) (68.669, 30.303)
  , CubicBezier (68.669, 30.303) (86.035, 49.372) (94.573, 74.786) (100.0, 100.0)
  , Close (100.0, 100.0)
  , CubicBezier
      (100.0, 100.0)
      (102.8, 113.012)
      (105.111, 126.162)
      (109.086, 138.864)
  , CubicBezier
      (109.086, 138.864)
      (113.06, 151.566)
      (118.873, 163.861)
      (127.566, 173.939)
  , CubicBezier
      (127.566, 173.939)
      (136.259, 184.017)
      (147.801, 191.583)
      (160.49, 195.601)
  , CubicBezier (160.49, 195.601) (173.178, 199.619) (186.69, 200.0) (200.0, 200.0)
  , Close (200.0, 200.0)
  , CubicBezier
      (200.0, 200.0)
      (220.922, 200.0)
      (242.348, 201.061)
      (262.295, 194.751)
  , CubicBezier
      (262.295, 194.751)
      (272.269, 191.596)
      (281.731, 186.31)
      (288.726, 178.532)
  , CubicBezier
      (288.726, 178.532)
      (295.721, 170.754)
      (300.0, 160.461)
      (300.0, 150.0)
  , Close (300.0, 150.0)
  , CubicBezier
      (300.0, 150.0)
      (300.0, 139.539)
      (295.721, 129.246)
      (288.726, 121.468)
  , CubicBezier
      (288.726, 121.468)
      (281.731, 113.69)
      (272.269, 108.404)
      (262.295, 105.249)
  , CubicBezier (262.295, 105.249) (242.348, 98.939) (220.922, 100.0) (200.0, 100.0)
  , Close (200.0, 100.0)
  , CubicBezier (200.0, 100.0) (193.509, 100.0) (186.884, 99.72) (180.83, 97.378)
  , CubicBezier
      (180.83, 97.378)
      (174.777, 95.036)
      (169.548, 90.765)
      (165.746, 85.505)
  , CubicBezier
      (165.746, 85.505)
      (161.943, 80.245)
      (159.493, 74.145)
      (157.284, 68.041)
  , CubicBezier (157.284, 68.041) (155.074, 61.938) (152.978, 55.767) (150.0, 50.0)
  , Close (150.0, 50.0)
  , CubicBezier (150.0, 50.0) (144.557, 39.46) (135.934, 30.934) (126.725, 23.456)
  , CubicBezier (126.725, 23.456) (117.516, 15.979) (108.388, 8.388) (100.0, 0.0)
  , Close (100.0, 0.0)
  , CubicBezier (100.0, 0.0) (86.601, -13.399) (74.424, -28.346) (67.072, -45.811)
  , CubicBezier (67.072, -45.811) (59.719, -63.275) (56.216, -82.1) (50.0, -100.0)
  , Close (50.0, -100.0)
  , CubicBezier (50.0, -100.0) (43.57, -118.518) (33.722, -136.001) (20.0, -150.0)
  ]
