{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Diagrams.Segment.SpiroSpline.Types
Copyright: (c) Marcellus Siegburg, 2025
License: MIT
Maintainer: marcellus.siegburg@uni-due.de

Provides the high level interface to use the underlying C library
by providing data types that have to be used in order to do so.
-}
module Diagrams.Segment.SpiroSpline.Types (
  -- * Input
  SpiroPoint (..),
  SpiroSpline (..),

  -- * Output
  SplineSegment (..),

  -- * Conversion functions

  {- | These convert interface data types into internal representations
  required by the underlying C library or vice versa.
  -}
  convertBezierSegment,
  fromSpiroSpline,
  toSpiroControlPoints,
) where

import Data.Char (chr, ord)
import Data.List.NonEmpty (NonEmpty, prependList, singleton, (<|))
import Diagrams.Prelude (P2, p2, unp2)
import GHC.Generics (Generic)

import Diagrams.Segment.SpiroSpline.FFI (
  BezierSegment (..),
  Four (Four, four, one, three, two),
  SpiroControlPoint (..),
 )

{- |
A spiro path consists of 'SpiroPoint's.
-}
data SpiroSpline point
  = -- | Start and Endpoint are not connected
    Open
      { startPoint :: point
      -- ^ the start point is just a point
      , spiroPoints :: NonEmpty (SpiroPoint point)
      -- ^ each 'SpiroPoint' has specific features
      , endPoint :: point
      -- ^ the end point is just a point
      }
  | -- | first and last point will be connected
    Closed {spiroPoints :: NonEmpty (SpiroPoint point)}
  deriving (Foldable, Functor, Generic, Show)

-- | Spiro spline points witch define a path.
data SpiroPoint point
  = {- | A point where curvatures and slopes of incoming and outgoing splines
    are unconstrained.
    -}
    SpiroCorner point
  | -- | Curve point that is continuous up to the fourth derivative.
    SpiroCurveG4 point
  | -- | Curve point that is continuous up to the second derivative.
    SpiroCurveG2 point
  | -- | A left constraint point to connect a curve to a straight line.
    SpiroLeftConstraint point
  | -- | A right constraint point to connect a curve to a straight line.
    SpiroRightConstraint point
  | {- | A knot point with fixed angle.
    The provided handle point creates the angle.
    It behaves as both left and right constraint point.
    -}
    SpiroAnchor
      { anchor :: point
      -- ^ the position of the knot point
      , handle :: point
      -- ^ the handle belonging to the knot point, creating the angle
      }
  deriving (Eq, Foldable, Functor, Generic, Show)

{- |
Takes a 'SpiroPoint' and transforms it into possibly multiple internal
 t'SpiroControlPoint's.
-}
toSpiroControlPoints :: SpiroPoint (P2 Double) -> [SpiroControlPoint]
toSpiroControlPoints = \case
  SpiroCorner point -> [pointWithTag point 'v']
  SpiroCurveG4 point -> [pointWithTag point 'o']
  SpiroCurveG2 point -> [pointWithTag point 'c']
  SpiroLeftConstraint point -> [pointWithTag point '[']
  SpiroRightConstraint point -> [pointWithTag point ']']
  SpiroAnchor {..} -> [pointWithTag anchor 'a', pointWithTag handle 'h']
  where
    pointWithTag point spiroTypeFlag =
      let (x, y) = unp2 point
      in SpiroControlPoint
           { spiroX = realToFrac x
           , spiroY = realToFrac y
           , spiroType = fromIntegral $ ord spiroTypeFlag
           }

{- |
Given a t'SpiroSpline' transform it into the internal representation.
-}
fromSpiroSpline
  :: SpiroSpline (P2 Double)
  -> NonEmpty SpiroControlPoint
fromSpiroSpline path =
  addPrefix
    (prependList (concatMap toSpiroControlPoints points) $ singleton suffix)
  where
    points = spiroPoints path
    (addPrefix, suffix) = case path of
      Open {..} ->
        ( (spiroTag '{' startPoint <|)
        , spiroTag '}' endPoint
        )
      Closed {} ->
        ( id
        , spiroTag 'z' $ p2 (0, 0)
        )
    spiroTag :: Char -> P2 Double -> SpiroControlPoint
    spiroTag c pt =
      SpiroControlPoint
        { spiroX = realToFrac $ fst $ unp2 pt
        , spiroY = realToFrac $ snd $ unp2 pt
        , spiroType = fromIntegral (ord c)
        }

{- |
Transform the internal C library bezier segment representation
into the higher level 'SplineSegment'.
-}
convertBezierSegment :: BezierSegment -> SplineSegment
convertBezierSegment BezierSegment {..} =
  case chr $ fromIntegral bezierSegmentType of
    'm' -> MoveTo two
    'l' -> LineTo one two
    'q' -> QuadraticBezier one two three
    'c' -> CubicBezier one two three four
    'k' -> Close one
    t -> error $ "Unknown tag: " ++ show t
  where
    Four {..} = fmap realToFrac <$> bezierSegmentCoordinates

{- |
A spline segment. For details see C library documentation.
-}
data SplineSegment
  = -- | where to continue
    MoveTo (P2 Double)
  | -- | a line from the first point to the second
    LineTo (P2 Double) (P2 Double)
  | -- | a quadratic bezier curve
    QuadraticBezier (P2 Double) (P2 Double) (P2 Double)
  | -- | a cubic bezier curve
    CubicBezier (P2 Double) (P2 Double) (P2 Double) (P2 Double)
  | -- | some end marker
    Close (P2 Double)
  deriving Show
