package ChessServer;

import server.Server

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length < 1) {
            usage
        } else {
            val s = new Server(new Config(args(0)));
            s.init
        }
    }

    def usage = {
        println("Usage: chessserver <config.xml>");
    }
}
