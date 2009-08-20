package ChessServer.logic

abstract class ChessTeam;

object White extends ChessTeam {
    override def toString = "White";
}
object Black extends ChessTeam {
    override def toString = "Black";
}
