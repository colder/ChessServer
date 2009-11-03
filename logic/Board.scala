package ChessServer.logic


import scala.collection.immutable.{HashMap,HashSet,Set,Map,TreeHashMap};


/* immutable */
case class Board(val slots: TreeHashMap[Position, Piece], val lastMove: Move) {
    type Slots = TreeHashMap[Position, Piece]
    /**
     * Public Board API
     */

    /* Returns the king of the color specified */
    def king(color: ChessTeam) = if (color == White) kingWhite else kingBlack

    /* Returns the piece at the position specified */
    def pieceAt(pos: Position): Option[Piece] = slots get pos;

    /* Returns if a position is free */
    def isFree(pos: Position) = !(slots contains pos)

    /* Checks if a path to the position is free */
    def isFreePath(pi: Piece, pos: Position) = pi.pos.pathTo(pos) forall isFree

    /* Checks if the position is occupied by the opponent */
    def isForeign(pi: Piece, pos: Position) = slots get pos match {
        case Some(po) => po.color != pi.color
        case _ => false
    }

    /* Checks if the king of the specified color is checkmate */
    def isCheckMate(color: ChessTeam) = {
        !isSafeFor(king(color)) && slots.values.filter{ _.color == color }.map{ movesOptionsCheckKingSafety(_) }.forall{ _.size == 0 }
    }

    /* Checks if a piece is safe where it is */
    def isSafeFor(pi: Piece): Boolean = isSafe(pi.color, pi.pos);
    
    /* Checks if a position is safe */
    def isSafe(targetColor: ChessTeam, target: Position): Boolean = {
        !(slots.values filter { _.color != targetColor } exists { basicMovesOptionsFor(_) contains target })
    }

    /* Returns the set of possible moves, excluding the ones
     * that endanger the King */
    def movesOptionsCheckKingSafety(pi: Piece): Set[Position] = movesOptionsFor(pi) filter { pos =>
        val newBoard = performMoveUnchecked(pi, pos).board

        newBoard.isSafeFor(newBoard.king(pi.color))
    }

    /* Move the pieces around, which might require multiple actions
     * i.e. castling or en passant */
    def performMove(posFrom: Position, posTo: Position): MoveResult = {
        slots get posFrom match {
            case Some(pi) =>
                // check validity of the target
                if (!(movesOptionsCheckKingSafety(pi) contains posTo)) {
                    throw new BoardException("You can't move there!")
                }

                performMoveUnchecked(pi, posTo);
            case None =>
                throw new BoardException("Invalid move: no piece found at "+posFrom);
        }
    }


    /* Returns the positions at which the piece is allowed to move in the
     * board, including castling */
    def movesOptionsFor(pi: Piece): Set[Position] = {
        var positions = basicMovesOptionsFor(pi)
        pi.typ match {
            case King =>
                if (!pi.hasMoved) {
                    for (rookPosX <- List(1,8)) {
                        val rookPos = Position(rookPosX, pi.pos.y)
                        pieceAt(rookPos) match {
                            case Some(po) if !po.hasMoved =>
                                val d = if (rookPosX == 1) -1 else 1;
                                val path = List(pi.pos.offset(d,0), pi.pos.offset(2*d,0))

                                if (isSafe(pi.color, pi.pos) && path.forall {pos => isSafe(pi.color, pos) && isFree(pos)}) {
                                    positions += pi.pos.offset(2*d, 0)
                                }
                            case _ =>
                        }
                    }
                }

            case _ =>
        }

        positions
    }

    /* Returns the board with the pawn promoted */
    def promote(pi: Piece, typeTo: PieceType): Board = {
        if (pi.typ != Pawn) {
            throw new BoardException("You cannot promote a piece that is not a pawn");
        }

        if (typeTo != Queen && typeTo != Rook && typeTo != Bishop && typeTo != Knight) {
            throw new BoardException("Invalid promotion. You can't promote to that kind of piece.");
        }

        if (pi.color == White && pi.pos.y != 8 || pi.color == Black && pi.pos.y != 1) {
            throw new BoardException("The piece is not in a position at which it can be promoted!");
        }

        val newSlots = Board.placePiece(removePiece(slots, pi), pi.color, typeTo, pi.pos);
        Board(newSlots, lastMove)
    }

    def serialize: String = {
        var pieces: List[String] = Nil
        for (i <- 1 to 8) {
            for (j <- 1 to 8) {
                slots.get(Position(i, j)) match {
                    case Some(p) => val color = p.color.toString; pieces = pieces ::: color(0)+p.typ.ab+p.pos.algNotation :: Nil
                    case None =>
                }
            }
        }

        val bs = pieces mkString ","

        val wc = king(White).moved match {
            case 0 =>
                val lr = pieceAt(Position("a1")) match {
                    case Some(Piece(White, Rook, _, 0)) => "y"
                    case _ => "n"
                }
                val rr = pieceAt(Position("h1")) match {
                    case Some(Piece(White, Rook, _, 0)) => "y"
                    case _ => "n"
                }

                lr+rr

            case _ => "nn"
        }
        val bc = king(Black).moved match {
            case 0 =>
                val lr = pieceAt(Position("a8")) match {
                    case Some(Piece(Black, Rook, _, 0)) => "y"
                    case _ => "n"
                }
                val rr = pieceAt(Position("h8")) match {
                    case Some(Piece(Black, Rook, _, 0)) => "y"
                    case _ => "n"
                }

                lr+rr
            case _ => "nn"
        }

        bs + wc + bc
    }

    /**
     * Private Board helpers
     */

    private val kingBlack = slots.values find { pi => pi.typ == King && pi.color == Black } match {
        case Some(pi) => pi
        case None => throw new BoardException("Can't find any black kings in the pieces!")
    }

    private val kingWhite = slots.values find { pi => pi.typ == King && pi.color == White } match {
        case Some(pi) => pi
        case None => throw new BoardException("Can't find any white kings in the pieces!")
    }

    private def movePiece(posFrom: Position, posTo: Position): Board = pieceAt(posFrom) match {
        case Some(pi) => movePiece(pi, posTo)
        case None => throw new BoardException("Slot "+posFrom+" contains no piece!")

    }
    private def movePiece(pi: Piece, posTo: Position): Board = pieceAt(posTo) match {
        case Some(op) => throw new BoardException("Slot "+posTo.algNotation+" is already occupied by "+op)
        case None => pieceAt(pi.pos) match {
            case Some(op) if op == pi =>
                Board((slots - pi.pos) ++ List((posTo, pi.move(posTo))), Move(pi, posTo))
            case _ => throw new BoardException("Warning! Can't move "+pi+" to "+posTo+"! Board is out of sync: Slot "+pi.pos.algNotation+" is not occupied by "+pi)
        }
    }

    private def capturePiece(pi: Piece): Board = Board(removePiece(slots, pi), lastMove)

    private def removePiece(slots: Slots, pt: Piece): Slots = slots get pt.pos match {
        case Some(op) if (op == pt) =>
            slots - pt.pos
        case _ => throw new BoardException("Warning! Can't remove! Board is out of sync: Slot "+pt.pos.algNotation+" is not occupied by "+pt)
    }

    private def moveOrCapture(pi: Piece, pos: Position): Board = slots get pos match {
        case Some(op) => capturePiece(op).movePiece(pi, pos)
        case None => movePiece(pi, pos)
    }


    /* calculates the positions at which the piece is allowed to move in the board */
    private def basicMovesOptionsFor(pi: Piece): Set[Position] = {
        val positions = pi.typ match {
            case Knight =>
                pi.basicMoveOptions filter { x => isFree(x) || isForeign(pi,x) }
            case Pawn =>
                var pos: List[Position] = pi.basicMoveOptions filter { pos => isFree(pos) }

                // can capture in diagonal
                val y = pi.color match { case White => pi.pos.y+1 case Black => pi.pos.y-1 } 
                pos :::= (for(x <- List(pi.pos.x-1, pi.pos.x+1) if (Position.isValid(x,y) && isForeign(pi, Position(x,y)))) yield Position(x,y));
                // en passant
                if (pi.pos.y == 5 && pi.color == White || pi.pos.y == 4 && pi.color == Black) {
                    val y = if (pi.color == White) pi.pos.y+1 else pi.pos.y-1
                    // check right
                    if (Position.isValid(pi.pos.x+1, pi.pos.y) && isForeign(pi, pi.pos.offset(+1,0))) {
                        //Check that the right slot is 1) a pawn, 2) the last move was him moving 2 slots
                        if (lastMove.piece.typ == Pawn && (List(2,7) contains lastMove.piece.pos.y)) {
                            pos = Position(pi.pos.x+1, y) :: pos
                        }
                    }

                    // check left
                    if (Position.isValid(pi.pos.x-1, pi.pos.y) && isForeign(pi, pi.pos.offset(-1,0))) {
                        //Check that the left slot is 1) a pawn, 2) the last move was him moving 2 slots
                        if (lastMove.piece.typ == Pawn && (List(2,7) contains lastMove.piece.pos.y)) {
                            pos = Position(pi.pos.x-1, y) :: pos
                        }

                    }
                }

                pos
            case _ => pi.basicMoveOptions filter { pos => (isForeign(pi, pos) || isFree(pos)) && isFreePath(pi,pos) }
        }

        Set[Position]()++positions
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
                    MoveResult(newboard, piece.typ == Pawn, piece.typ != Pawn)
            }
        }
    }
}

object Board {
    type Slots = TreeHashMap[Position, Piece]

    /* Returns the slots with the newly created piece in it */
    def placePiece(slots: Slots, color: ChessTeam, typ: PieceType, pos: Position): Slots = slots get pos match {
        case Some(op) => throw new BoardException("Position "+pos.algNotation+" is already occupied by "+op)
        case None => slots(pos) = Piece(color, typ, pos, 0)
    }

    /* Initializes a chess game */
    def init = {
        var slots = TreeHashMap[Position, Piece]()

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


        Board(slots, Move(Piece(White, King, Position(5,8), 0), Position(5,8)))
    }

}

case class BoardException(override val msg: String) extends GameException(msg);

case class MoveResult(board: Board, reinitBoards: Boolean, incMovesWC: Boolean)
