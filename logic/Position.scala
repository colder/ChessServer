package ChessServer.logic




/**
 * Position:
 * x : The x axis (1 = a , .... 8 = h)
 * y : The y axis (1, ... , 8)
 */


case class Position(val x: Int, val y: Int) {

    def this(str: String) = this(str(0)-'a'+1, str(1)-'1'+1)

    def algNotation = ('a'+(x-1)).toChar+""+y;

    def left = Position(x+1, y)
    def right = Position(x-1, y)
    def up = Position(x, y+1)
    def down = Position(x, y-1)

    def offset(ox: Int, oy: Int) = Position(x+ox, y+oy)

    def pathTo(to: Position) = Position.path(this, to)

    if (!Position.isValid(x,y)) throw new IllegalPositionException(x, y)
}

object Position {
    def isValid(x: Int, y: Int) = x > 0 && x < 9 && y > 0 && y < 9
    def isValidOffset(p: Position, ox: Int, oy: Int) = isValid(p.x+ox, p.y+oy)

    def apply(str: String): Position = new Position(str(0)-'a'+1, str(1)-'1'+1)

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

}


class IllegalPositionException(x: Int, y: Int) extends RuntimeException("Position("+x+","+y+") is out of range");
class InvalidPositionException(str: String) extends RuntimeException("Position("+str+") is invalid");

case class Move(piece: Piece, posTo: Position);
