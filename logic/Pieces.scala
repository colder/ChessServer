package ChessServer.logic

abstract class PieceType(val ab: String)
case object King extends PieceType("K")
case object Queen extends PieceType("Q")
case object Rook extends PieceType("R")
case object Bishop extends PieceType("B")
case object Knight extends PieceType("N")
case object Pawn extends PieceType("P")

/* Immutable */
case class Piece(val color: ChessTeam, val typ: PieceType, val pos: Position, val moved: Int) {

    def move(p: Position) = Piece(color, typ, p, moved+1)

    override def toString = {
        "["+color+" "+typ+" "+pos+"]"
    }

    def hasMoved = moved > 0

    /* Basic moves that do not depend on other pieces 
     * Some moves might be removed or added depending on the confuguration of the board
     */
    def basicMoveOptions: List[Position] = typ match {
        case King =>
            for (x <- List(0, 1, -1);
                 y <- List(0, 1, -1)
                    if (x != 0 || y != 0)
                        && Position.isValidOffset(pos, x, y)) yield pos.offset(x,y)
        case Queen =>
            for (x <- List(0, 1, -1);
                 y <- List(0, 1, -1);
                 n <- 1 to 8
                    if (x != 0 || y != 0)
                        && Position.isValidOffset(pos, n*x, n*y)) yield pos.offset(n*x,n*y)
        case Rook =>
            for (x <- List(0, 1); y <- List(0, 1); n <- -8 to 8
                    if x != y && n != 0
                       && Position.isValidOffset(pos, n*x, n*y)) yield pos.offset(n*x, n*y)
        case Bishop =>
            for (x <- List(-1, 1); y <- List(-1, 1); n <- 1 to 8
                    if Position.isValidOffset(pos, n*x, n*y)) yield pos.offset(n*x, n*y)
        case Knight =>
            for (x <- List(-1, -2, 1, 2); y <- List(-1, -2, 1, 2)
                    if x*x!=y*y && Position.isValidOffset(pos, x, y)) yield  pos.offset(x, y)
        case Pawn => 
            if (pos.y == 1 || pos.y == 8)
                Nil
            else color match {
                case White => Position(pos.x, pos.y+1) :: (if (pos.y == 2) Position(pos.x, pos.y+2) :: Nil else Nil)
                case Black => Position(pos.x, pos.y-1) :: (if (pos.y == 7) Position(pos.x, pos.y-2) :: Nil else Nil)
            }
    }
}
