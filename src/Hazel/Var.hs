{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | An adaptation of the locally nameless representation (See Charguéraud's
-- "The Locally Nameless Representation" for more information) which uses 2D
-- de Bruijn variables.

module Hazel.Var where

import Control.Lens (Iso', Traversal', iso)
import Data.Word (Word32)

import qualified Data.Text as T

-- | The distance in binding levels between a variable and its binder. The
-- "first dimension" in our 2D de Bruijn scheme.
newtype Depth
  = Depth { _depth :: Word32 }
  deriving (Eq, Show, Enum)

unDepth :: Iso' Depth Word32
unDepth = iso _depth Depth

-- | A binder slot. The second dimension in our 2D de Bruijn scheme.
newtype Slot
  = Slot { _slot :: Word32 }
  deriving (Eq, Show)

unSlot :: Iso' Slot Word32
unSlot = iso _slot Slot

-- | A 2-dimensional locally-nameless variable.
data Variable
  = B { _boundDepth :: {-# UNPACK #-} !Depth
      , _boundSlot  :: {-# UNPACK #-} !Slot }
  | F { _freeName   :: {-# UNPACK #-} !T.Text }
  deriving (Eq, Show)

boundDepth :: Traversal' Variable Depth
boundDepth f (B depth slot) = fmap (\ depth' -> B depth' slot) (f depth)
boundDepth _ (F name) = pure (F name)

boundSlot :: Traversal' Variable Slot
boundSlot f (B depth slot) = fmap (\ slot' -> B depth slot') (f slot)
boundSlot _ (F name) = pure (F name)

freeName :: Traversal' Variable T.Text
freeName _ (B depth slot) = pure (B depth slot)
freeName f (F name) = fmap F (f name)
