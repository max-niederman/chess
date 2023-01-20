{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Board where

import           Chess.Piece
import           Control.Monad (msum)
import           Data.Bits
import           Data.Vector   (iforM)
import qualified Data.Vector   as V
import           Data.Word     (Word64)
import           Numeric       (showHex)
import           Text.Printf   (printf)

newtype Bitboard =
  Bitboard Word64
  deriving (Eq, Bits)

instance Show Bitboard where
  show (Bitboard w) = printf "Bitboard 0x%016x" w

data Board =
  Board
    { whitePawns   :: Bitboard
    , whiteKnights :: Bitboard
    , whiteBishops :: Bitboard
    , whiteRooks   :: Bitboard
    , whiteQueens  :: Bitboard
    , whiteKing    :: Bitboard
    , blackPawns   :: Bitboard
    , blackKnights :: Bitboard
    , blackBishops :: Bitboard
    , blackRooks   :: Bitboard
    , blackQueens  :: Bitboard
    , blackKing    :: Bitboard
    }
  deriving (Eq)

getPieceAt :: Board -> Int -> Maybe Piece
getPieceAt b i =
  msum $
  map
    ($ i)
    [ getPiece (Piece White Pawn) (whitePawns b)
    , getPiece (Piece Black Pawn) (blackPawns b)
    , getPiece (Piece White Rook) (whiteRooks b)
    , getPiece (Piece Black Rook) (blackRooks b)
    , getPiece (Piece White Bishop) (whiteBishops b)
    , getPiece (Piece Black Bishop) (blackBishops b)
    , getPiece (Piece White Knight) (whiteKnights b)
    , getPiece (Piece Black Knight) (blackKnights b)
    , getPiece (Piece White King) (whiteKing b)
    , getPiece (Piece Black King) (blackKing b)
    , getPiece (Piece White Queen) (whiteQueens b)
    , getPiece (Piece Black Queen) (blackQueens b)
    ]
  where
    getPiece :: Piece -> Bitboard -> Int -> Maybe Piece
    getPiece p b i =
      if testBit b i
        then Just p
        else Nothing

pieces :: Board -> V.Vector (Maybe Piece)
pieces b = V.generate 64 $ getPieceAt b

showBoardPretty :: Board -> String
showBoardPretty b =
  unlines $ map (foldMap showPieceFEN) $ reverse $ parts 8 $ pieces b
  where
    parts :: Int -> V.Vector a -> [V.Vector a]
    parts n v =
      if V.length v < n
        then []
        else let (h, t) = V.splitAt n v
              in h : parts n t


starting :: Board
starting =
  Board
    { whitePawns = Bitboard 0x000000000000ff00
    , whiteKnights = Bitboard 0x0000000000000042
    , whiteBishops = Bitboard 0x0000000000000024
    , whiteRooks = Bitboard 0x0000000000000081
    , whiteQueens = Bitboard 0x0000000000000008
    , whiteKing = Bitboard 0x0000000000000010
    , blackPawns = Bitboard 0x00ff000000000000
    , blackKnights = Bitboard 0x4200000000000000
    , blackBishops = Bitboard 0x2400000000000000
    , blackRooks = Bitboard 0x8100000000000000
    , blackQueens = Bitboard 0x0800000000000000
    , blackKing = Bitboard 0x1000000000000000
    }
