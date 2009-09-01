package ChessServer;

import server.Server

object Main {
    def main(args: Array[String]): Unit = {
        val s = new Server(12345);

        s.start
    }
}
