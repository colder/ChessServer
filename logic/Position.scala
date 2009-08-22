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


    if (!Position.isValid(x,y)) throw new IllegalPositionException(x, y)
}

object Position {
    def isValid(x: Int, y: Int) = x > 0 && x < 9 && y > 0 && y < 9
    def isValidOffset(p: Position, ox: Int, oy: Int) = isValid(p.x+ox, p.y+oy)
}


class IllegalPositionException(x: Int, y: Int) extends RuntimeException("Position("+x+","+y+") is out of range");
class InvalidPositionException(str: String) extends RuntimeException("Position("+str+") is invalid");

case class Move(piece: Piece, posTo: Position);
