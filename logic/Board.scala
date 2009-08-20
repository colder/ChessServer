package ChessServer.logic


import scala.collection.mutable.HashMap;

class Board {

    /* Careful! Pieces need to stay in sync with the board */
    private var capturedPieces: List[Piece] = Nil;
    private var slots: HashMap[Position, Piece] = new HashMap[Position, Piece]();


    def placePiece(color: ChessTeam, typ: PieceType, pos: Position) = slots get pos match {
        case Some(op) => throw new BoardException("Position "+pos.algNotation+" is already occupied by "+op)
        case None => slots(pos) = new Piece(color, typ, pos)
    }

    def movePiece(pi: Piece, posTo: Position) = slots get posTo match {
        case Some(op) => throw new BoardException("Slot "+posTo.algNotation+" is already occupied by "+op)
        case None => slots get pi.pos match {
            case Some(op) if op == pi =>
                slots -= pi.pos
                pi.move(posTo)
                slots(posTo) = pi

            case _ => throw new BoardException("Warning! Board is out of sync: Slot "+pi.pos.algNotation+" is not occupied by "+pi)
        }
    }

    def capturePiece(pt: Piece) = slots get pt.pos match {
        case Some(op) if (op == pt) =>
            slots -= pt.pos
            capturedPieces = pt :: capturedPieces
        case _ => throw new BoardException("Warning! Board is out of sync: Slot "+pt.pos.algNotation+" is not occupied by "+pt)
    }


    def initBoard = {
        // Pawns
        for (x <- 1 to 8) {
            placePiece(White, Pawn, Position(x, 2))
            placePiece(Black, Pawn, Position(x, 7))
        }

        // Rooks
        for (x <- List(1,8)) {
            placePiece(White, Rook, Position(x, 1))
            placePiece(Black, Rook, Position(x, 8))
        }

        // Knights
        for (x <- List(2,7)) {
            placePiece(White, Knight, Position(x, 1))
            placePiece(Black, Knight, Position(x, 8))
        }

        // Bishops
        for (x <- List(3,6)) {
            placePiece(White, Bishop, Position(x, 1))
            placePiece(Black, Bishop, Position(x, 8))
        }

        // Queens
        placePiece(White, Queen, Position(4, 1))
        placePiece(Black, Queen, Position(4, 8))

        // Kings
        placePiece(White, King, Position(5, 1))
        placePiece(Black, King, Position(5, 8))


    }

    def draw = {
        def line = println("     +-----+-----+-----+-----+-----+-----+-----+------")

        line
        for (yb <- 1 to 8) {
            val y = 9-yb

            var l1 = "     ";
            var l2 = "  "+y+"  ";
            var l3 = "     ";

            for (x <- 1 to 8) {
                l1 = l1 + "|     "
                l2 = l2 + (slots.get(Position(x,y)) match {
                    case Some(p) => "|  "+p.typ.ab+"  ";
                    case None => "|     ";
                })
                l3 = l3 + "|     "
            }
            println(l1+"|");
            println(l2+"|");
            println(l3+"|");
            line
        }

        println;
        println("     "+((0 to 7) map { x: Int => "   "+('A'+x).toChar+"  " }).mkString)

    }

}

class BoardException(m: String) extends RuntimeException(m);
