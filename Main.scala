package ChessServer;

object Main {
    def main(args: Array[String]): Unit = {
        val c = new client.CLIClient();

        c.start
    }
}
