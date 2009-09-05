package ChessServer.server

import java.net.ServerSocket;
import scala.collection.mutable.HashMap

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    var games = new HashMap[String, ServerGame]()

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

    def auth(username: String, password: String, seed: String): Boolean = {
        // TODO
        true
    }

    def create(client: ServerClient, timers: Long): ServerGame = {
        if (games contains client.username) {
            throw ServerException("Already playing")
        } else {
            val game = ServerGame(client, timers)
            games(client.username) = game
            game
        }
    }

    def join(client: ServerClient, host: String): ServerGame = {
        if (games contains client.username) {
            throw ServerException("Already playing")
        } else {
            games.get(host) match {
                case Some(g) =>
                    if (g.started) {
                        throw ServerException("Game already started")
                    } else {
                        g.join(client)
                        g
                    }

                case None =>
                    throw ServerException("Game not found")
            }
        }
    }

}

case class ServerException(msg: String) extends RuntimeException(msg)

