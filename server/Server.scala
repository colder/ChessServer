package ChessServer.server

import java.net.ServerSocket;
import scala.collection.mutable.{HashMap,HashSet}

import database._
import java.sql.SQLException

class Server(cfg: Config) {
    val serverSocket = new ServerSocket(cfg.hostPort)

    /* Stores every logged users: username->client */
    var users   = new HashMap[String, ServerClient]()

    /* Stores every pending games: (host)->HashSet[game] */
    var pendingGames = new HashMap[String, HashSet[ServerGame]]()

    /* Stores every running games: (host, opponent)->game */
    var games   = new HashMap[(String, String), ServerGame]()

    /* Stores every players: username->Set[game] */
    var players = new HashMap[String, HashSet[ServerGame]]()

    /* Database connection */
    val db = new MysqlConnection(cfg.dbDatabase, cfg.dbUser, cfg.dbPass)

    /* Server port */
    val port = cfg.hostPort

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

    def login(client: ServerClient, username: String, challenge: String, seed: String): Option[Int] = {
        try {
            val stmt = db.prepareStatement("SELECT id, password, logged_in FROM users WHERE username = ?", username)
            val results = stmt.executeQuery

            val retval = if (results.hasNext) {
                val res = results.firstRow
                if (Hash.sha1(res.getString("password")+seed) equals challenge) {
                    if (res.getString("logged_in") equals "yes") {
                        throw new ServerException("Login already in use");
                    }
                    db.prepareStatement("UPDATE users SET date_lastlogin=NOW(), logged_in = 'yes' WHERE id = ?", res.getInt("id")).executeUpdate

                    users(username) = client
                    players(username) = new HashSet[ServerGame]()
                    pendingGames(username) = new HashSet[ServerGame];

                    Some(res.getInt("id"))
                } else {
                    None
                }
            } else {
                None
            }

            stmt.close
            retval
        } catch {
            case ex: SQLException =>
                println("Woops: "+ex);
                None
        }
    }

    def logout(client: ServerClient) = {
        if (client.userid > 0) {
            try {
                db.prepareStatement("UPDATE users SET logged_in = 'no' WHERE id = ?", client.userid).executeUpdate
            } catch {
                case ex: SQLException =>
                    println("Woops: "+ex);
            }
        }
        users -= client.username
        players(client.username).foreach { _.resign(client) }
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
        if (games.get((host, client.username)) != None || games.get((client.username, host)) != None) {
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

    val positions = new HashMap[String, GPSPosition]();

    def registerGPS(client: ServerClient, long: Int, lat: Int) = {
        println("GPS: "+client.username+" ("+long+", "+lat+")");
        positions(client.username) = GPSPosition(long, lat)
    }

    def getGPS(username: String): Option[GPSPosition] = {
        positions.get(username)
    }
}

case class ServerException(msg: String) extends RuntimeException(msg)

