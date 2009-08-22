package ChessServer.logic


import scala.collection.immutable.{HashMap,HashSet,Set};


/* immutable */
case class Board(val turn: ChessTeam, val slots: Map[Position, Piece], val capturedPieces: List[Piece]) {
    type Slots = Map[Position, Piece]

    /*
     * Functions used by the controller to make the game evolve
     */
    def movePiece(posFrom: Position, posTo: Position): Board = slots get posFrom match {
        case Some(p) => movePiece(p, posTo)
        case None => throw new BoardException("Slot "+posFrom+" contains no piece!")

    }
    def movePiece(pi: Piece, posTo: Position): Board = slots get posTo match {
        case Some(op) => throw new BoardException("Slot "+posTo.algNotation+" is already occupied by "+op)
        case None => slots get pi.pos match {
            case Some(op) if op == pi =>
                Board(turn, (slots - pi.pos)(posTo) = pi.move(posTo), capturedPieces)
            case _ => throw new BoardException("Warning! Board is out of sync: Slot "+pi.pos.algNotation+" is not occupied by "+pi)
        }
    }

    def nextTurn: Board = Board(if (turn == White) Black else White, slots, capturedPieces)

    def capturePiece(pt: Piece): Board = Board(turn, removePiece(slots, pt), pt :: capturedPieces)

    def removePiece(slots: Slots, pt: Piece): Slots = slots get pt.pos match {
        case Some(op) if (op == pt) =>
            slots - pt.pos
        case _ => throw new BoardException("Warning! Board is out of sync: Slot "+pt.pos.algNotation+" is not occupied by "+pt)
    }



    /* returns the positions in the path between two slots */
    def path(from: Position, to: Position) = {
        def dx = if (from.x > to.x) -1 else if (from.x < to.x) 1 else 0;
        def dy = if (from.y > to.y) -1 else if (from.y < to.y) 1 else 0;

        var positions: List[Position] = Nil
        var nx = from.x+dx
        var ny = from.y+dy

        while ((dx*nx < dx*to.x || dy*ny < dy*to.y) && Position.isValid(nx, ny)) {
            positions ::= Position(nx,ny)
            nx += dx
            ny += dy

        }

        positions
    }


    /* returns if a position is free */
    def isFree(pos: Position) = !(slots contains pos)

    /* checks if a path to the position is free */
    def isFreePath(p: Piece, pos: Position) = path(p.pos, pos) forall isFree

    /* checks if the position is occupied by the opponent */
    def isForeign(p: Piece, pos: Position) = slots get pos match {
        case Some(po) => po.color != p.color
        case _ => false
    }

    /* calculates the positions at which the piece is allowed to move in the board */
    def movesOptionsFor(p: Piece): Map[Position, Action] = {
        val posActions = p.typ match {
            case Knight =>
                p.basicMoveOptions filter { x => isFree(x) || isForeign(p,x) } map { (_, Normal) }

            case Pawn =>
                var pos = p.basicMoveOptions filter { pos => isFree(pos) } map { (_, Normal) }

                // can capture in diagonal
                val y = p.color match { case White => p.pos.y+1 case Black => p.pos.y-1 } 
                pos ::: (for(x <- List(p.pos.x-1, p.pos.x+1) if (Position.isValid(x,y) && isForeign(p, Position(x,y)))) yield (Position(x,y), Normal));

                // en passant
                // TODO
            case King =>
                p.basicMoveOptions filter { pos => (isForeign(p, pos) || isFree(pos)) } map { (_, Normal) }
                // TODO: castling

            case Rook =>
                p.basicMoveOptions filter { pos => (isForeign(p, pos) || isFree(pos)) && isFreePath(p,pos) } map { (_, Normal) }
                // TODO: castling

            case _ => p.basicMoveOptions filter { pos => (isForeign(p, pos) || isFree(pos)) && isFreePath(p,pos) } map { (_, Normal) }
        }

        Map[Position, Action]()++posActions
    }

}

object Board {

    type Slots = Map[Position, Piece]

    def placePiece(slots: Slots, color: ChessTeam, typ: PieceType, pos: Position): Slots = slots get pos match {
        case Some(op) => throw new BoardException("Position "+pos.algNotation+" is already occupied by "+op)
        case None => slots(pos) = Piece(color, typ, pos)
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


        Board(White, slots, Nil)
    }

}
class BoardException(m: String) extends RuntimeException(m);
