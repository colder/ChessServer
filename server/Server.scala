package ChessServer.server

import java.net.ServerSocket;
import scala.collection.mutable.{HashMap,HashSet}

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    /* Stores every logged users: username->client */
    var users   = new HashMap[String, ServerClient]()

    /* Stores every pending games: (host)->HashSet[game] */
    var pendingGames = new HashMap[String, HashSet[ServerGame]]()

    /* Stores every running games: (host, opponent)->game */
    var games   = new HashMap[(String, String), ServerGame]()

    /* Stores every players: username->Set[game] */
    var players = new HashMap[String, HashSet[ServerGame]]()

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

    def login(client: ServerClient, username: String, challenge: String, seed: String): Boolean = {
        if (Hash.sha1("plop"+seed) equals challenge) {
            if (users.get(username) != None) {
                throw new ServerException("Login already in use");
            }
            users(username) = client
            players(username) = new HashSet[ServerGame]()
            pendingGames(username) = new HashSet[ServerGame];
            true
        } else {
            false
        }
    }

    def logout(client: ServerClient) = {
        users -= client.username
        players -= client.username
        pendingGames -= client.username
    }

    def create(client: ServerClient, timers: Long): ServerGame = {
        val game = new ServerGame(this, client, timers)

        pendingGames(client.username) += game;
        players(client.username) += game

        game
    }

    def join(client: ServerClient, host: String, timers: Long): ServerGame = {
        if (games.get((host, client.username)) != None) {
            throw ServerException("Already playing against "+host+"!")
        }
        if (host equals client.username) {
            throw ServerException("Can't join your own game!")
        }

        pendingGames.get(host) match {
            case Some(gs) =>
                    gs.find { _.ts == timers } match {
                        case Some(g) =>
                            g.join(client)

                            games((host, client.username)) = g
                            pendingGames(host) -= g;
                            players(client.username) += g

                            g
                        case None =>
                            throw ServerException("Game not found")
                    }
            case None =>
                throw ServerException("Game not found")
        }
    }

    def leave(client: ServerClient) = {
        if (client.status == Logged) {
            players(client.username).foreach { _.resign(client) }
            logout(client)
        }
    }

    def gameEnd(servergame: ServerGame) = {
        val hostUsername = servergame.host.username
        players(hostUsername) -= servergame

        servergame.host.onGameEnd(servergame)

        servergame.opponent match {
            case Some(sc) => {
                // remove running game
                sc.onGameEnd(servergame)
                games -= ((hostUsername, sc.username))

                players(sc.username) -= servergame
            }
            case None =>
                // remove pending game
                pendingGames(hostUsername) -= servergame
        }
    }

    def freeGames(client: ServerClient) = {
        pendingGames.values.map{_.toList}.reduceLeft{_:::_}
    }

}

case class ServerException(msg: String) extends RuntimeException(msg)

