package ChessServer;

object MainClient {
    def main(args: Array[String]): Unit = {
        val c = new client.CLIClient;

        c.start
    }
}
