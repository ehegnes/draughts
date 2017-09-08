-- |
-- Module: Draughts.Library.BoardSpec
-- Copyright: (C) 2017 Siddhanathan Shanmugam
-- License: GPL (see the file LICENSE)
-- 
-- Maintainer: siddhanathan@gmail.com
-- Stability: experimental
-- Portability: non-portable (GHC Extensions)
--
-- Test specifications for the Draughts Board
--

module Draughts.Library.BoardSpec (main, spec) where

import Draughts.Library.Types
import Draughts.Library.Board
import Draughts.Library.Board.Internal

import Data.Array  (listArray, (!))
import Data.List   (sort)
import Data.Monoid ((<>))
import Test.Hspec  (describe, hspec, SpecWith, Spec, shouldBe, it)

playerMovesSpecGen :: (Player, Int, [Int]) -> SpecWith ()
playerMovesSpecGen (p, i, os) =
    it name $ term `shouldBe` expectedValue
  where
    name = show i <> " returned " <> show os
    term = sort . playerMoves (Piece p Man) . Index $ i
    expectedValue = fmap Index . sort $ os

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    playerTransitionsSpec
    playerMovesSpec
    singleJumpSpec
    crownSpec

crownSpec :: Spec
crownSpec = describe "Crowning" $ do
    let b1 = Board $ listArray (Index 1, Index 32) $ mconcat
                [ replicate 24 Nothing
                , [Nothing, Just (Piece Black Man), Nothing, Nothing]
                , replicate 4 Nothing
                ]
        Just (b2, _) = makeMove b1 Black (Index 26) (Index 30)
        (Board b3) = crownBoard b2
    it "C1" $ b3 ! (Index 30) `shouldBe` Just (Piece Black King)

singleJumpSpec :: Spec
singleJumpSpec = describe "singleJump" $ do
    let b1 = Board $ listArray (Index 1, Index 32) $ mconcat
                [ replicate 12 Nothing
                , [Nothing, Just (Piece Black Man), Nothing, Nothing]
                , [Just (Piece White Man), Nothing, Nothing, Nothing]
                , replicate 12 Nothing
                ]
        r1 = singleJumps b1 (Index 14)
    it "SJ01" $ r1 `shouldBe` [(Index 17, Index 21)]

playerTransitionsSpec :: Spec
playerTransitionsSpec = describe "playerTransitions" $ do
    let b1 = initialBoard
        r1 = sort $ playerTransitions b1 Black (Index 9)
        Just (b2, cs1) = makeMove b1 Black (Index 9) (Index 14)
        b2x = removePiece (Index 9) cs1 (Index 14) b1

        r2 = sort $ playerTransitions b2 White (Index 21)
        Just (b3, cs2) = makeMove b2 White (Index 21) (Index 17)
        b3x = removePiece (Index 21) cs2 (Index 17) b2

        r3 = sort $ playerTransitions b3 Black (Index 14)
        Just (b4, cs3) = makeMove b3 Black (Index 14) (Index 21)
        b4x = removePiece (Index 14) cs3 (Index 21) b3

        sj = singleJumps b3 (Index 14)
    it "P1R1" $ r1 `shouldBe` [Index 13, Index 14]
    it "P1C1" $ cs1 `shouldBe` []
    it "MR01" $ b2 `shouldBe` b2x
    it "P2R1" $ r2 `shouldBe` [Index 17]
    it "P2C1" $ cs2 `shouldBe` []
    it "MR02" $ b3 `shouldBe` b3x
    it "P1R2" $ r3 `shouldBe` [Index 21]
    it "P1C2" $ cs3 `shouldBe` [Index 17]
    it "MR03" $ b4 `shouldBe` b4x
    it "SJ01" $ sj `shouldBe` [(Index 17, Index 21)]

playerMovesSpec :: Spec
playerMovesSpec = describe "playerMoves" $ do
    playerMovesSpecGen (Black,  1, [ 5,  6])
    playerMovesSpecGen (Black,  2, [ 6,  7])
    playerMovesSpecGen (Black,  3, [ 7,  8])
    playerMovesSpecGen (Black,  4, [ 8]    )
    playerMovesSpecGen (Black,  5, [ 9]    )
    playerMovesSpecGen (Black,  6, [ 9, 10])
    playerMovesSpecGen (Black,  7, [10, 11])
    playerMovesSpecGen (Black,  8, [11, 12])
    playerMovesSpecGen (Black,  9, [13, 14])
    playerMovesSpecGen (Black, 10, [14, 15])
    playerMovesSpecGen (Black, 11, [15, 16])
    playerMovesSpecGen (Black, 12, [16]    )
    playerMovesSpecGen (Black, 13, [17]    )
    playerMovesSpecGen (Black, 14, [17, 18])
    playerMovesSpecGen (Black, 15, [18, 19])
    playerMovesSpecGen (Black, 16, [19, 20])
    playerMovesSpecGen (Black, 17, [21, 22])
    playerMovesSpecGen (Black, 18, [22, 23])
    playerMovesSpecGen (Black, 19, [23, 24])
    playerMovesSpecGen (Black, 20, [24]    )
    playerMovesSpecGen (Black, 21, [25]    )
    playerMovesSpecGen (Black, 22, [25, 26])
    playerMovesSpecGen (Black, 23, [26, 27])
    playerMovesSpecGen (Black, 24, [27, 28])
    playerMovesSpecGen (Black, 25, [29, 30])
    playerMovesSpecGen (Black, 26, [30, 31])
    playerMovesSpecGen (Black, 27, [31, 32])
    playerMovesSpecGen (Black, 28, [32]    )
    playerMovesSpecGen (Black, 29, []      )
    playerMovesSpecGen (Black, 30, []      )
    playerMovesSpecGen (Black, 31, []      )
    playerMovesSpecGen (Black, 32, []      )
    playerMovesSpecGen (White,  1, []      )
    playerMovesSpecGen (White,  2, []      )
    playerMovesSpecGen (White,  3, []      )
    playerMovesSpecGen (White,  4, []      )
    playerMovesSpecGen (White,  5, [ 1]    )
    playerMovesSpecGen (White,  6, [ 1,  2])
    playerMovesSpecGen (White,  7, [ 2,  3])
    playerMovesSpecGen (White,  8, [ 3,  4])
    playerMovesSpecGen (White,  9, [ 5,  6])
    playerMovesSpecGen (White, 10, [ 6,  7])
    playerMovesSpecGen (White, 11, [ 7,  8])
    playerMovesSpecGen (White, 12, [ 8]    )
    playerMovesSpecGen (White, 13, [ 9]    )
    playerMovesSpecGen (White, 14, [ 9, 10])
    playerMovesSpecGen (White, 15, [10, 11])
    playerMovesSpecGen (White, 16, [11, 12])
    playerMovesSpecGen (White, 17, [13, 14])
    playerMovesSpecGen (White, 18, [14, 15])
    playerMovesSpecGen (White, 19, [15, 16])
    playerMovesSpecGen (White, 20, [16]    )
    playerMovesSpecGen (White, 21, [17]    )
    playerMovesSpecGen (White, 22, [17, 18])
    playerMovesSpecGen (White, 23, [18, 19])
    playerMovesSpecGen (White, 24, [19, 20])
    playerMovesSpecGen (White, 25, [21, 22])
    playerMovesSpecGen (White, 26, [22, 23])
    playerMovesSpecGen (White, 27, [23, 24])
    playerMovesSpecGen (White, 28, [24]    )
    playerMovesSpecGen (White, 29, [25]    )
    playerMovesSpecGen (White, 30, [25, 26])
    playerMovesSpecGen (White, 31, [26, 27])
    playerMovesSpecGen (White, 32, [27, 28])
