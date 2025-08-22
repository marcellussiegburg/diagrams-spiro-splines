{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module: Diagrams.Segment.SpiroSpline.FFI
Copyright: (c) Marcellus Siegburg, 2025
License: MIT
Maintainer: marcellus.siegburg@uni-due.de

Uses a foreign function interface for
<https://github.com/fontforge/libspiro/ libspiro>
in order to convert spiro control points into bezier segments.

For more information on spiro splines see also the documentation
of the above linked C library.
-}
module Diagrams.Segment.SpiroSpline.FFI (
  BezierSegment (..),
  Four (..),
  SpiroControlPoint (..),
  runSpiro,
) where

import Control.Monad (when)
import Data.Bits (Bits ((.|.)))
import Data.List.NonEmpty (NonEmpty, toList)
import Diagrams.Prelude (P2)
import Foreign.C.Types (CChar, CDouble, CInt (..))
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (alignment, peek, poke, sizeOf))
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic, Generic1)

-- | t'Bezctx' will be handled internally by the library.
pattern SPIRO_INTERNAL_BEZCTX :: CInt
pattern SPIRO_INTERNAL_BEZCTX = 0x0200

-- | Defines the default mode
pattern SPIRO_CUBIC_TO_BEZIER :: CInt
pattern SPIRO_CUBIC_TO_BEZIER = 0x0000

{- |
A t'SpiroControlPoint' is a point as defined by the C library.
A spiro spline consists of such points.

This low-level type is just used to provide the data for the foreign-function
interface.
-}
data SpiroControlPoint = SpiroControlPoint
  { spiroX :: !CDouble
  -- ^ horizontal position
  , spiroY :: !CDouble
  -- ^ vertical position
  , spiroType :: !CChar
  -- ^ type as specified by the library
  }
  deriving (GStorable, Generic, Show)

-- | Allocate memory for a t'LsBezctx' that later on will contain the result.
foreign import ccall "new_ls_bezctx"
  c_new_ls_bezctx
    :: CInt
    -- ^ expected size
    -> CInt
    -- ^ flags
    -> IO (Ptr LsBezctx)

-- | Free previous allocated memory of a t'LsBezctx'.
foreign import ccall "free_ls_bezctx"
  c_free_ls_bezctx :: Ptr LsBezctx -> IO ()

-- | Convert control points into bezier curve using t'Bezctx'.
foreign import ccall "TaggedSpiroCPsToBezier2"
  c_TaggedSpiroCPsToBezier2
    :: Ptr SpiroControlPoint
    -> CInt
    -> Ptr Bezctx
    -> IO CInt

{- |
Something having four components of same type.
-}
data Four a = Four
  { one :: !a
  -- ^ first
  , two :: !a
  -- ^ second
  , three :: !a
  -- ^ third
  , four :: !a
  -- ^ fourth
  }
  deriving (Eq, Functor, GStorable, Generic, Generic1, Show)

{- |
A segment of the resulting path as returned by the C library.
-}
data BezierSegment = BezierSegment
  { bezierSegmentType :: !CChar
  -- ^ The type of segment is indicated by a letter
  , bezierSegmentCoordinates :: !(Four (P2 CDouble))
  {- ^ Coordinates belonging to the segment,
  depending on the type some might be irrelevant.
  -}
  }
  deriving (GStorable, Generic, Show)

{- |
Bezctx is some internal structure of libspiro which is processed by the library.
We ignore it.
-}
newtype Bezctx = Bezctx ()

{- |
This instance just ignores all bytes that some t'Bezctx' occupies in memory.
-}
instance Storable Bezctx where
  sizeOf Bezctx {} = 40
  alignment Bezctx {} = 40
  peek _ = pure (Bezctx ())
  poke _ (Bezctx ()) = pure ()

{- |
This structure is provided by the library.
It is used to convert control points into bezier segments
and finally to retrieve them.
-}
data LsBezctx = LsBezctx
  { _bezctx :: !Bezctx
  -- ^ the library handles this
  , segmentsPointer :: !(Ptr BezierSegment)
  -- ^ the actual result
  , numberOfSegments :: !CInt
  -- ^ the number of resulting segments
  }
  deriving (GStorable, Generic)

{- |
The core internal function converting t'SpiroControlPoint's
to t'BezierSegment's by calling the libspiro C implementation
using the internal handling of t'Bezctx'.
-}
runSpiro :: NonEmpty SpiroControlPoint -> IO [BezierSegment]
runSpiro controlPoints = do
  withArray (toList controlPoints) $ \controlPointsPointer -> do
    bezctx <- c_new_ls_bezctx lengthOverApproximation SPIRO_INTERNAL_BEZCTX
    when (bezctx == nullPtr)
      $ fail "Failed to allocate memory"
    success <-
      c_TaggedSpiroCPsToBezier2
        controlPointsPointer
        (SPIRO_CUBIC_TO_BEZIER .|. SPIRO_INTERNAL_BEZCTX)
        (castPtr bezctx :: Ptr Bezctx)
    when (success == 0) $ do
      c_free_ls_bezctx bezctx
      fail "Creating bezier from Spiro Control Points failed"
    segments <- peek bezctx
    bezierSegments <-
      peekArray
        (fromIntegral (numberOfSegments segments))
        $ segmentsPointer segments
    c_free_ls_bezctx bezctx
    pure bezierSegments
  where
    n = fromIntegral (length controlPoints)
    lengthOverApproximation = n * 10
