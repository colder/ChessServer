package ChessServer.server

import java.net.ServerSocket;
import scala.collection.mutable.{HashMap,HashSet}

import database._
import java.sql.SQLException

class Server(cfg: Config) {
    val serverSocket = new ServerSocket(cfg.hostPort)

    /* Stores every logged users: username->client */
    var users   = new HashMap[String, ServerClient]()

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
        init
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
    }

    def create(client: ServerClient, player: String, timers: Long): Result[_ <: (ServerGame, ServerClient)] = {
        if (games.get((player, client.username)) != None || games.get((player, client.username)) != None) {
            Failure("Already playing against "+player+"!")
        } else if (player equals client.username) {
            Failure("Can't create to play against yourself!")
        } else {
            users.get(player) match {
                case Some(opp) =>
                    val game = new ServerGame(this, client, opp, timers)

                    games((client.username, player)) = game;
                    players(client.username) += game
                    players(opp.username) += game

                    Success((game, opp))
                case None =>
                    Failure("Opponent not found")
            }
        }
    }

    def inviteaccept(client: ServerClient, host: String): Result[_ <: ServerGame] = {
        games.get((host, client.username)) match {
            case Some(g) =>
                g.inviteaccept
                Success(g)
            case None =>
                Failure("Game not found")
        }
    }

    def invitedecline(client: ServerClient, host: String): Result[_ <: ServerGame] = {
        games.get((host, client.username)) match {
            case Some(g) =>
                g.invitedecline
                gameEnd(g)
                Success(g)
            case None =>
                Failure("Game not found")
        }
    }

    def leave(client: ServerClient) = {
        if (client.status == Logged) {
            logout(client)
        }
    }

    def gameEnd(servergame: ServerGame) = {
        val hostUsername = servergame.host.username
        val oppUsername = servergame.opponent.username

        players(hostUsername) -= servergame
        players(oppUsername) -= servergame

        servergame.host.onGameEnd(servergame)
        servergame.opponent.onGameEnd(servergame)

        games -= ((hostUsername, oppUsername))
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

    def logChatMessage(from: ServerClient, to: ServerClient, msg: String) = {
        try {
            db.prepareStatement("""INSERT INTO chat
                                         SET id_user_from = ?,
                                             id_user_to = ?,
                                             date = NOW(),
                                             message = ?""", from.userid, to.userid, msg).executeUpdate
        } catch {
            case ex: SQLException =>
                println("Woops: "+ex);
        }
    }

    def init = {
        Runtime.getRuntime().addShutdownHook(new Thread {
            override def run = shutdown
        })
    }

    def shutdown = {
        println("Shutting down gracefully..")
        users map { u => leave(u._2) }
    }
}

case class ServerException(msg: String) extends RuntimeException(msg)

