{-# LANGUAGE LambdaCase #-}

{- |
Module: Diagrams.Segment.SpiroSpline
Description: Provides spiro splines for diagrams using a FFI to libspiro
Copyright: (c) Marcellus Siegburg, 2025
License: MIT
Maintainer: marcellus.siegburg@uni-due.de

Provides the ability to define spiro splines
in order to create diagrams trails.

It uses a foreign function interface to essentially call
<https://github.com/fontforge/libspiro/ libspiro>
and then draw the resulting lines and bezier curves.

For more information on spiro splines see also the documentation
of the above linked C library.
-}
module Diagrams.Segment.SpiroSpline (
  -- * Spiro Splines

  {- | A spiro spline ('SpiroSpline') consists of
  spiro control points ('SpiroPoint').
  These are used to define the spline and are required.
  Closed splines are beginning and ending in the same point
  whereas open splines end in different points.
  A possible viewpoint can be that a closed spine describes a figure
  and an open spline describes a curve.
  -}
  SpiroSpline (..),
  SpiroPoint (..),
  spiroSplineToTrail,
) where

import Data.Maybe (mapMaybe)
import Diagrams.Prelude (
  Affine ((.+^), (.-.)),
  Closed,
  P2,
  Segment,
  Trail,
  V2,
  bezier3,
  closeTrail,
  fromSegments,
  straight,
  (*^),
 )

import Diagrams.Segment.SpiroSpline.Internal.FFI (runSpiro)
import Diagrams.Segment.SpiroSpline.Internal.Types (
  SpiroPoint (..),
  SpiroSpline (..),
  SplineSegment (..),
  convertBezierSegment,
  fromSpiroSpline,
 )

{- |
Converts a 'SplineSegment' returned by the C library to a diagrams 'Segment'.

'Nothing' on 'MoveTo' or 'Close'.
-}
segmentToDiagrams
  :: SplineSegment (P2 Double)
  -- ^ segment to process
  -> Maybe (Segment Closed V2 Double)
segmentToDiagrams = \case
  MoveTo _ -> Nothing
  LineTo point1 point2 ->
    Just (straight (point2 .-. point1))
  QuadraticBezier point1 point2 point3 ->
    -- convert a quadratic bezier to a cubic one see
    -- https://stackoverflow.com/q/3162645
    let
      c1 = point1 .+^ ((2 / 3) *^ (point2 .-. point1))
      c2 = point3 .+^ ((2 / 3) *^ (point2 .-. point3))
    in
      Just (bezier4 point1 c1 c2 point3)
  CubicBezier point1 point2 point3 point4 ->
    Just (bezier4 point1 point2 point3 point4)
  Close _ -> Nothing
  where
    bezier4 point1 point2 point3 point4 =
      bezier3 (point2 .-. point1) (point3 .-. point1) (point4 .-. point1)

{- |
Convert a 'SplineSegment' retrieved from libspiro to a diagrams 'Trail'.
-}
segmentsToTrail :: [SplineSegment (P2 Double)] -> Trail V2 Double
segmentsToTrail = fromSegments . mapMaybe segmentToDiagrams

{- |
The core function of this library.
Takes a 'SpiroSpline' and converts it into a diagrams 'Trail'.
-}
spiroSplineToTrail
  :: SpiroSpline (P2 Double)
  -> IO (Trail V2 Double)
spiroSplineToTrail spiroSpline = do
  let controlPoints = fromSpiroSpline spiroSpline
  bezierSegments <- runSpiro controlPoints
  let close = case spiroSpline of
        Closed {} -> closeTrail
        Open {} -> id
  let splineSegments = map convertBezierSegment bezierSegments
  pure $ close $ segmentsToTrail splineSegments
