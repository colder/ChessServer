package ChessServer.server

import java.net.ServerSocket;
import scala.collection.mutable.HashMap

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    /* Stores every logged users: username->client */
    var users   = new HashMap[String, ServerClient]()

    /* Stores every hosts: username->game */
    var games   = new HashMap[String, ServerGame]()

    /* Stores every players: username->game */
    var players = new HashMap[String, ServerGame]()

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

    def login(client: ServerClient, username: String, challenge: String, seed: String): Boolean = {
        if (Hash.sha1("plop"+seed) equals challenge) {
            users(username) = client
            true
        } else {
            false
        }
    }

    def logout(client: ServerClient) = {
        users -= client.username
    }

    def create(client: ServerClient, timers: Long): ServerGame = {
        if (players contains client.username) {
            throw ServerException("Already playing")
        } else {
            val game = ServerGame(this, client, timers)
            games(client.username) = game
            players(client.username) = game
            game
        }
    }

    def join(client: ServerClient, host: String): ServerGame = {
        if (players contains client.username) {
            throw ServerException("Already playing")
        } else {
            games.get(host) match {
                case Some(g) =>
                    if (g.started) {
                        throw ServerException("Game already started")
                    } else {
                        g.join(client)
                        players(client.username) = g
                        g
                    }

                case None =>
                    throw ServerException("Game not found")
            }
        }
    }

    def leave(client: ServerClient) = {
        players.get(client.username) match {
            case Some(game) =>
                // Leaving an unfinished game
                game.resign(client)
            case None =>
                // Leaving normally
        }
        users -= client.username
    }

    def gameEnd(servergame: ServerGame) = {
        games -= servergame.host.username
        players -= servergame.host.username

        servergame.host.onGameEnd

        servergame.opponent match {
            case Some(sc) => {
                sc.onGameEnd
                players -= sc.username
            }
            case None =>
        }
    }

}

case class ServerException(msg: String) extends RuntimeException(msg)

