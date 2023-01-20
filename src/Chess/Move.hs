module Chess.Move where

import Chess.Board (Board, getPieceAt)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- A move is represented by a pair of integers, the first being the
-- source square and the second being the destination square.
data Move =
  Move Int Int
  deriving (Eq, Show, Read)

