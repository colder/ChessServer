package ChessServer.logic

abstract class PieceType(val ab: String);
object King extends PieceType("K");
object Queen extends PieceType("Q");
object Rook extends PieceType("R");
object Bishop extends PieceType("B");
object Knight extends PieceType("N");
object Pawn extends PieceType("P");

class Piece(val color: ChessTeam, val typ: PieceType, initPos: Position) {
    var pos = initPos;
    var lastPos = initPos

    def move(p: Position) = {
        lastPos = pos
        pos = p
    }

    override def toString = {
        color+" "+typ
    }

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
            for (x <- List(0, 1); y <- List(0, 1); n <- List(-8, 8)
                    if x != y && n != 0
                       && Position.isValidOffset(pos, n*x, n*y)) yield pos.offset(n*x, n*y)
        case Bishop =>
            for (x <- List(-1, 1); y <- List(-1, 1); n <- List(1, 8)
                    if Position.isValidOffset(pos, n*x, n*y)) yield pos.offset(n*x, n*y)
        case Knight =>
            for (x <- List(-1, -2, 1, 2); y <- List(-1, -2, 1, 2)
                    if x*x!=y*y && Position.isValidOffset(pos, x, y)) yield  pos.offset(x, y)
        case Pawn => 
            if (pos.y == 0 || pos.y == 8)
                Nil
            else color match {
                case White => Position(pos.x, pos.y+1) :: (if (pos.y == 2) Position(pos.x, pos.y+2) :: Nil else Nil)
                case Black => Position(pos.x, pos.y-1) :: (if (pos.y == 7) Position(pos.x, pos.y-2) :: Nil else Nil)
            }
    }
}
