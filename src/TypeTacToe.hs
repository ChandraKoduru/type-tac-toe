{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module TypeTacToe where

import Data.Function ((&))

-- | Either X, O or Nothing
data PieceT = X | O | N
  deriving (Show, Eq)

data Trip a = Trip a a a
  deriving (Show, Eq, Functor)

-- | Keep a list of each Piece played and its location
data BoardRep = Empty
              | Cons CoordT CoordT PieceT BoardRep

newtype Board (b :: BoardRep) a = Board (Trip (Trip a))
  deriving (Show, Eq, Functor)

newBoard :: Board 'Empty PieceT
newBoard = Board $ Trip (Trip N N N)
                        (Trip N N N)
                        (Trip N N N)

data CoordT = A | B | C
  deriving (Show, Eq)

-- | A proxy type which represents a coordinate
data Coord (a :: CoordT) where
  A' :: Coord 'A
  B' :: Coord 'B
  C' :: Coord 'C

-- | Get the coord's actual value from a wrapper type
coordVal :: Coord a -> CoordT
coordVal A' = A
coordVal B' = B
coordVal C' = C

-- | Utility function to alter a value inside a triple
-- Can set values using `const x`
overTrip :: CoordT -> (a -> a) -> Trip a -> Trip a
overTrip A f (Trip a b c) = Trip (f a) b c
overTrip B f (Trip a b c) = Trip a (f b) c
overTrip C f (Trip a b c) = Trip a b (f c)

-- | Has a square been played already ?
type family Played (x :: CoordT) (y :: CoordT) (b :: BoardRep) :: Bool where
  Played _ _ 'Empty = 'False
  Played x y ('Cons x y _ _) = 'True
  Played x y ('Cons _ _ _ rest) = Played x y rest

-- | whose turn is it now ?
type family Turn (b :: BoardRep) :: PieceT where
  Turn ('Cons _ _ 'X _) = 'O
  Turn _ = 'X

-- | playX only when the cell is not occupied and is x's turn
playX :: (Played x y b ~ 'False, Turn b ~ 'X) => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT
playX (coordVal -> x, coordVal -> y) (Board b) 
  = Board $ overTrip y (overTrip x (const X)) b

-- | playX only when the cell is not occupied and is y's turn
playO :: (Played x y b ~ 'False, Turn b ~ 'O) => (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
playO (coordVal -> x, coordVal -> y) (Board b)
  = Board $ overTrip y (overTrip x (const O)) b

game1 = newBoard & playX (A', B')
game2 = newBoard & playX (A', B') & playO (A', C')

-- | invalid moves

-- wrong turn. When un-commented, the module will not compile
-- game3 = newBoard & playX (A', B') & playX (A', C')

-- playing into an occupied cell. When un-commented, the module will not compile
-- game4 = newBoard & playX (A', B') & playO (A', B')

main :: IO ()
main = do
  print (show game1)
  print ("O-making move")
  print (show (game1 & playO (A', C')))
