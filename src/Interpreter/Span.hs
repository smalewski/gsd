module Interpreter.Span ( Span(..), HasSpan(..), (.>), showSpan ) where

import Prelude hiding (span)
import qualified Data.Text as T

data Span = Span { beginPos :: Int, endPos :: Int }
  deriving (Eq, Ord)

(.>) :: Int -> Span -> Span
(.>) begin endSpan = Span begin begin <> endSpan

instance Show Span where
  show (Span from to) = show from <> " " <> show to

showSpan :: Span -> T.Text
showSpan = T.pack . show

instance Semigroup Span where
  (Span begin1 end1) <> (Span begin2 end2) =
    Span (min begin1 begin2) (max end1 end2)

instance Monoid Span where
  mempty = Span maxBound minBound

class HasSpan a where
  span :: a -> Span

instance HasSpan Span where
  span = id

instance HasSpan a => HasSpan (a, b) where
  span (a, _) = span a

instance HasSpan a => HasSpan (a, b, c) where
  span (a, _, _) = span a

instance HasSpan a => HasSpan (a, b, c, d) where
  span (a, _, _, _) = span a

instance HasSpan a => HasSpan [a] where
  span = mconcat . fmap span
