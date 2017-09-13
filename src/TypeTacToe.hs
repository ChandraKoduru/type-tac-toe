{-# LANGUAGE DeriveFunctor #-}
module TypeTacToe where

-- | Either X, O or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

data Trip a = Trip a a a
  deriving (Show, Eq, Functor)

newtype Board a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

newBoard :: Board PieceT
newBoard = Board $ Trip (Trip N N N)
                        (Trip N N N)
                        (Trip N N N)

data CoordT = A | B | C
  deriving (Show, Eq)

-- | Utility function to alter a value inside a triple
-- Can set values using `const x`
overTrip :: CoordT -> (a -> a) -> Trip a -> Trip a
overTrip A f (Trip a b c) = Trip (f a) b c
overTrip B f (Trip a b c) = Trip a (f b) c
overTrip C f (Trip a b c) = Trip a b (f c)

play :: PieceT -> (CoordT, CoordT) -> Board PieceT -> Board PieceT
play p (x, y) (Board b) = Board $ overTrip y (overTrip x (const p)) b

