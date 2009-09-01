package ChessServer

import java.net.ServerSocket;

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

}

