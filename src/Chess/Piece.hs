module Chess.Piece where

data Color
  = White
  | Black
  deriving (Eq, Show, Read)

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq, Show, Read)

data Piece =
  Piece Color PieceType
  deriving (Eq, Show, Read)

pieceTypeLetter :: PieceType -> Char
pieceTypeLetter pt =
  case pt of
    Pawn   -> 'P'
    Knight -> 'N'
    Bishop -> 'B'
    Rook   -> 'R'
    Queen  -> 'Q'
    King   -> 'K'

showPieceGlyph :: Piece -> String
showPieceGlyph p =
  case p of
    Piece White Pawn   -> "♙"
    Piece White Knight -> "♘"
    Piece White Bishop -> "♗"
    Piece White Rook   -> "♖"
    Piece White Queen  -> "♕"
    Piece White King   -> "♔"
    Piece Black Pawn   -> "♟"
    Piece Black Knight -> "♞"
    Piece Black Bishop -> "♝"
    Piece Black Rook   -> "♜"
    Piece Black Queen  -> "♛"
    Piece Black King   -> "♚"

showPieceFEN :: Piece -> String
showPieceFEN p =
  case p of
    Piece White Pawn   -> "P"
    Piece White Knight -> "N"
    Piece White Bishop -> "B"
    Piece White Rook   -> "R"
    Piece White Queen  -> "Q"
    Piece White King   -> "K"
    Piece Black Pawn   -> "p"
    Piece Black Knight -> "n"
    Piece Black Bishop -> "b"
    Piece Black Rook   -> "r"
    Piece Black Queen  -> "q"
    Piece Black King   -> "k"
