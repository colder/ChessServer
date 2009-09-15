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

    def login(client: ServerClient, username: String, challenge: String, seed: String): Result[_ <: Int] = {
        try {
            val stmt = db.prepareStatement("SELECT id, password, logged_in FROM users WHERE username = ?", username)
            val results = stmt.executeQuery

            val retval = if (results.hasNext) {
                val res = results.firstRow
                if (Hash.sha1(res.getString("password")+seed) equals challenge) {
                    if (res.getString("logged_in") equals "yes") {
                        Failure("Login already in use")
                    } else {
                        db.prepareStatement("UPDATE users SET date_lastlogin=NOW(), logged_in = 'yes' WHERE id = ?", res.getInt("id")).executeUpdate

                        users(username) = client
                        players(username) = new HashSet[ServerGame]()
                        pendingGames(username) = new HashSet[ServerGame];

                        Success(res.getInt("id"))
                    }
                } else {
                    Failure("Incorrect username/password")
                }
            } else {
                Failure("Incorrect username/password")
            }

            stmt.close
            retval
        } catch {
            case ex: SQLException =>
                Failure("Woops: "+ex);
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

    def findPendingGame(host: String, timers: Long): Result[_ <: ServerGame] = {
        pendingGames.get(host) match {
            case Some(gs) =>
                    gs.find { _.ts == timers } match {
                        case Some(g) =>
                            Success(g)
                        case None =>
                            Failure("Game not found with those timers")
                    }
            case None =>
                Failure("This host doesn't have any pending game")
        }
    }

    def join(client: ServerClient, host: String, timers: Long): Result[_ <: ServerGame] = {
        if (games.get((host, client.username)) != None || games.get((client.username, host)) != None) {
            Failure("Already playing against "+host+"!")
        }
        if (host equals client.username) {
            Failure("Can't join your own game!")
        }

        findPendingGame(host, timers) match {
            case Success(g) =>
                g.join(client)

                games((host, client.username)) = g
                pendingGames(host) -= g;
                players(client.username) += g

                Success(g)
            case f :Failure =>
                f
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

    def registerGPS(client: ServerClient, long: Int, lat: Int) = {
        try {
            db.prepareStatement("""INSERT INTO gps_positions
                                         SET id_user = ?,
                                             longitude = ?,
                                             latitude = ?,
                                             date = NOW()""", client.userid, long, lat).executeUpdate
        } catch {
            case ex: SQLException =>
                println("Woops: "+ex);
        }
    }

    def getGPS(username: String): Option[GPSPosition] = {
        try {
            val stmt = db.prepareStatement("""SELECT longitude, latitude
                                                FROM gps_positions gp, users u
                                               WHERE id_user = u.id AND u.username = ?
                                            ORDER BY date DESC
                                               LIMIT 1""", username)
            val results = stmt.executeQuery

            val retval = if (results.hasNext) {
                val res = results.firstRow
                Some(GPSPosition(res.getInt("longitude"), res.getInt("latitude")))
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
}

case class ServerException(msg: String) extends RuntimeException(msg)

