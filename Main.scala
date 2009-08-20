package ChessServer;

object Main {
    def main(args: Array[String]): Unit = {
        val b = new logic.Board;
        b.initBoard

        b.draw
    }
}
