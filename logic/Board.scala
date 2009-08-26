package ChessServer.logic


import scala.collection.immutable.{HashMap,HashSet,Set};


/* immutable */
case class Board(val slots: Map[Position, Piece], val capturedPieces: List[Piece], val lastMove: Move) {
    type Slots = Map[Position, Piece]

    val kingBlack = slots.values find { p => p.typ == King && p.color == Black } match {
        case Some(op) => op
        case None => throw new BoardException("Can't find any black kings in the pieces!")
    }

    val kingWhite = slots.values find { p => p.typ == King && p.color == White } match {
        case Some(op) => op
        case None => throw new BoardException("Can't find any white kings in the pieces!")
    }

    def king(color: ChessTeam) = if (color == White) kingWhite else kingBlack

    private def movePiece(posFrom: Position, posTo: Position): Board = slots get posFrom match {
        case Some(p) => movePiece(p, posTo)
        case None => throw new BoardException("Slot "+posFrom+" contains no piece!")

    }
    private def movePiece(pi: Piece, posTo: Position): Board = slots get posTo match {
        case Some(op) => throw new BoardException("Slot "+posTo.algNotation+" is already occupied by "+op)
        case None => slots get pi.pos match {
            case Some(op) if op == pi =>
                Board((slots - pi.pos)(posTo) = pi.move(posTo), capturedPieces, Move(pi, posTo))
            case _ => throw new BoardException("Warning! Board is out of sync: Slot "+pi.pos.algNotation+" is not occupied by "+pi)
        }
    }

    private def capturePiece(pt: Piece): Board = Board(removePiece(slots, pt), pt :: capturedPieces, lastMove)

    private def removePiece(slots: Slots, pt: Piece): Slots = slots get pt.pos match {
        case Some(op) if (op == pt) =>
            slots - pt.pos
        case _ => throw new BoardException("Warning! Board is out of sync: Slot "+pt.pos.algNotation+" is not occupied by "+pt)
    }

    private def moveOrCapture(p: Piece, pos: Position): Board = slots get pos match {
        case Some(op) => capturePiece(op).movePiece(p, pos)
        case None => movePiece(p, pos)
    }

    def pieceAt(pos: Position): Option[Piece] = slots get pos;

    /* returns if a position is free */
    def isFree(pos: Position) = !(slots contains pos)

    /* checks if a path to the position is free */
    def isFreePath(p: Piece, pos: Position) = p.pos.pathTo(pos) forall isFree

    /* checks if the position is occupied by the opponent */
    def isForeign(p: Piece, pos: Position) = slots get pos match {
        case Some(po) => po.color != p.color
        case _ => false
    }

    /* calculates the positions at which the piece is allowed to move in the board */
    def basicMovesOptionsFor(p: Piece): Set[Position] = {
        val positions = p.typ match {
            case Knight =>
                p.basicMoveOptions filter { x => isFree(x) || isForeign(p,x) }
            case Pawn =>
                var pos: List[Position] = p.basicMoveOptions filter { pos => isFree(pos) }

                // can capture in diagonal
                val y = p.color match { case White => p.pos.y+1 case Black => p.pos.y-1 } 
                pos :::= (for(x <- List(p.pos.x-1, p.pos.x+1) if (Position.isValid(x,y) && isForeign(p, Position(x,y)))) yield Position(x,y));
                pos
            case _ => p.basicMoveOptions filter { pos => (isForeign(p, pos) || isFree(pos)) && isFreePath(p,pos) }
        }

        Set[Position]()++positions
    }


    def movesOptionsFor(p: Piece): Set[Position] = {
        var positions = basicMovesOptionsFor(p)
        p.typ match {
            case Pawn =>
                // en passant
                if (p.pos.y == 5 && p.color == White || p.pos.y == 4 && p.color == Black) {
                    val y = if (p.color == White) p.pos.y+1 else p.pos.y-1
                    // check right
                    if (Position.isValid(p.pos.x+1, p.pos.y) && isForeign(p, p.pos.offset(+1,0))) {
                        //Check that the right slot is 1) a pawn, 2) the last move was him moving 2 slots
                        if (lastMove.piece.typ == Pawn && (List(2,7) contains lastMove.piece.pos.y)) {
                            positions += Position(p.pos.x+1, y)
                        }
                    }

                    // check left
                    if (Position.isValid(p.pos.x-1, p.pos.y) && isForeign(p, p.pos.offset(-1,0))) {
                        //Check that the left slot is 1) a pawn, 2) the last move was him moving 2 slots
                        if (lastMove.piece.typ == Pawn && (List(2,7) contains lastMove.piece.pos.y)) {
                            positions += Position(p.pos.x-1, y)
                        }

                    }
                }
            case King =>
                if (!p.hasMoved) {
                    for (rookPosX <- List(1,8)) {
                        val rookPos = Position(rookPosX, p.pos.y)
                        slots get rookPos match {
                            case Some(op) if !op.hasMoved =>
                                val d = if (rookPosX == 1) -1 else 1;
                                val path = List(p.pos.offset(d,0), p.pos.offset(2*d,0))

                                if (isSafe(p.color, p.pos) && path.forall {pos => isSafe(p.color, pos) && isFree(pos)}) {
                                    positions += p.pos.offset(2*d, 0)
                                }
                            case _ =>
                        }
                    }
                }

            case _ =>
        }

        positions
    }

    def isSafeFor(piece: Piece): Boolean = isSafe(piece.color, piece.pos);
    
    def isSafe(targetColor: ChessTeam, target: Position): Boolean = {
        !(slots.values filter { _.color != targetColor } exists { basicMovesOptionsFor(_) contains target })
    }

    def movesOptionsCheckKingSafety(piece: Piece): Set[Position] = movesOptionsFor(piece) filter { pos =>
        /* Only select moves that do not endanger the King */
        val newBoard = performMoveUnchecked(piece, pos).board

        newBoard.isSafeFor(newBoard.king(piece.color))
    }

    /* Complex perform move operation */
    def performMove(posFrom: Position, posTo: Position): MoveResult = {
        slots get posFrom match {
            case Some(p) =>
                // check validity of the target
                if (!(movesOptionsCheckKingSafety(p) contains posTo)) {
                    throw new BoardException("You can't move there!")
                }

                performMoveUnchecked(p, posTo);
            case None =>
                throw new BoardException("Invalid move: no piece found at "+posFrom);
        }
    }

    private def performMoveUnchecked(piece: Piece, posTo: Position): MoveResult = {
        // Special moves that requires two actions
        if (piece.typ == Pawn && posTo.x != piece.pos.x && isFree(posTo)) {
            // En passant
            slots get Position(posTo.x, piece.pos.y) match {
                case Some(op) =>
                    MoveResult(capturePiece(op).moveOrCapture(piece, posTo), true, false);
                case None =>
                    throw new BoardException("Invalid 'En passant': The slot targetted is empty")
            }
        }  else if (piece.typ == King && Math.abs(posTo.x-piece.pos.x) == 2) {
            // Castling
            val rookFromX = if (posTo.x > piece.pos.x) 8 else 1
            val rookToX   = if (posTo.x > piece.pos.x) 6 else 4
            slots get Position(rookFromX, piece.pos.y) match {
                case Some(op) =>
                    MoveResult(moveOrCapture(piece, posTo).moveOrCapture(op, Position(rookToX, piece.pos.y)), true, true);
                case None =>
                    throw new BoardException("Invalid castling! No rook to move")
            }
        } else {
            // Normal move/capture
            val newboard = moveOrCapture(piece, posTo)

            slots get posTo match {
                case Some(op) =>
                    MoveResult(newboard, true, false)
                case None =>
                    MoveResult(newboard, piece.typ == Pawn, true)
            }
        }
    }
}

object Board {

    type Slots = Map[Position, Piece]

    def placePiece(slots: Slots, color: ChessTeam, typ: PieceType, pos: Position): Slots = slots get pos match {
        case Some(op) => throw new BoardException("Position "+pos.algNotation+" is already occupied by "+op)
        case None => slots(pos) = Piece(color, typ, pos, 0)
    }

    def init = {
        var slots = HashMap[Position, Piece]()

        // Pawns
        for (x <- 1 to 8) {
            slots = placePiece(slots, White, Pawn, Position(x, 2))
            slots = placePiece(slots, Black, Pawn, Position(x, 7))
        }

        // Rooks
        for (x <- List(1,8)) {
            slots = placePiece(slots, White, Rook, Position(x, 1))
            slots = placePiece(slots, Black, Rook, Position(x, 8))
        }

        // Knights
        for (x <- List(2,7)) {
            slots = placePiece(slots, White, Knight, Position(x, 1))
            slots = placePiece(slots, Black, Knight, Position(x, 8))
        }

        // Bishops
        for (x <- List(3,6)) {
            slots = placePiece(slots, White, Bishop, Position(x, 1))
            slots = placePiece(slots, Black, Bishop, Position(x, 8))
        }

        // Queens
        slots = placePiece(slots, White, Queen, Position(4, 1))
        slots = placePiece(slots, Black, Queen, Position(4, 8))

        // Kings
        slots = placePiece(slots, White, King, Position(5, 1))
        slots = placePiece(slots, Black, King, Position(5, 8))


        Board(slots, Nil, Move(Piece(White, King, Position(5,8), 0), Position(5,8)))
    }

}

class BoardException(msg: String) extends GameException(msg);

case class MoveResult(board: Board, reinitBoards: Boolean, incMovesWC: Boolean)
