package ChessServer.server

import java.net.ServerSocket;
import scala.collection.mutable.{HashMap,HashSet}

import database._
import java.sql.SQLException

import scala.actors.Actor
import scala.actors.Actor._

class Server(cfg: Config) extends Actor {
    private val serverSocket = new ServerSocket(cfg.hostPort)

    /* Stores every logged users: username->client */
    private var users   = new HashMap[String, ServerClient]()

    /* Stores every running games: (host, opponent)->game */
    private var games   = new HashMap[(String, String), ServerGame]()

    /* Stores every players: username->Set[game] */
    private var players = new HashMap[String, HashSet[ServerGame]]()

    /* Database connection */
    private val db = new MysqlConnection(cfg.dbDatabase, cfg.dbUser, cfg.dbPass)

    /* Logger */
    val log = new TerminalLogger

    /* Server port */
    private val port = cfg.hostPort

    private def listen = {
        log.info("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

    private def login(client: ServerClient, username: String, challenge: String, seed: String): Result[_ <: Int] = {
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

    private def logout(client: ServerClient) = {
        if (client.userid > 0) {
            try {
                db.prepareStatement("UPDATE users SET logged_in = 'no' WHERE id = ?", client.userid).executeUpdate
            } catch {
                case ex: SQLException =>
                    log.err("Woops: "+ex);
            }

            users -= client.username
            players(client.username).foreach { _ ! Resign(client) }
            players -= client.username

            true
        } else {
            false
        }
    }

    private def create(client: ServerClient, player: String, timers: Long): Result[_ <: (ServerGame, ServerClient)] = {
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

                    opp ! OnGameInvite(game)

                    Success((game, opp))
                case None =>
                    Failure("Opponent not found")
            }
        }
    }

    private def leave(client: ServerClient) = {
        if (client.status == Logged) {
            logout(client)
        }
    }

    private def gameEnd(servergame: ServerGame) = {
        val hostUsername = servergame.host.username
        val oppUsername = servergame.opponent.username

        if (users contains hostUsername) {
            players(hostUsername) -= servergame
            servergame.host ! OnGameEnd(servergame)
        }

        if (users contains oppUsername) {
            players(oppUsername) -= servergame
            servergame.opponent ! OnGameEnd(servergame)
        }

        games -= ((hostUsername, oppUsername))

        servergame ! CloseGame
    }

    private def registerGPS(client: ServerClient, long: Int, lat: Int) = {
        try {
            db.prepareStatement("""INSERT INTO gps_positions
                                         SET id_user = ?,
                                             longitude = ?,
                                             latitude = ?,
                                             date = NOW()""", client.userid, long, lat).executeUpdate
        } catch {
            case ex: SQLException =>
                log.err("Woops: "+ex);
        }
    }

    private def getGPS(username: String): Option[GPSPosition] = {
        try {
            val stmt = db.prepareStatement("""SELECT longitude, latitude
                                                FROM gps_positions gp, users u
                                               WHERE id_user = u.id AND u.username = ?
                                            ORDER BY date DESC, gp.id DESC
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
                log.err("Woops: "+ex);
                None
        }
    }

    def init = {
        Runtime.getRuntime().addShutdownHook(new Thread {
            override def run = shutdown
        })

        start

        listen
    }

    def shutdown = {
        log.warn("Shutting down gracefully..")
        users map { u => leave(u._2) }
    }

    def act() {
        loop {
            receive {
                case LogChatMessage(from, to, msg) =>
                    try {
                        db.prepareStatement("""INSERT INTO chat
                                                     SET id_user_from = ?,
                                                         id_user_to = ?,
                                                         date = NOW(),
                                                         message = ?""", from.userid, to.userid, msg).executeUpdate
                    } catch {
                        case ex: SQLException =>
                            log.err("Woops: "+ex);
                    }
                case GetUser(username) =>
                    reply(users.get(username))
                case GameEnd(game) =>
                    gameEnd(game)
                case Leave(client) =>
                    reply(leave(client))
                case RegisterGPS(client, long, lat) =>
                    registerGPS(client, long, lat)
                case GetGPS(username) =>
                    reply(getGPS(username))
                case Login(client, username, challenge, salt) =>
                    reply(login(client, username, challenge, salt))
                case Logout(client) =>
                    reply(logout(client))
                case Create(client, username, timers) =>
                    reply(create(client, username, timers))
            }
        }
    }
}

abstract class ServerCommand;

case class LogChatMessage(from: ServerClient, to: ServerClient, msg: String) extends ServerCommand
case class GetUser(username: String) extends ServerCommand
case class GameEnd(game: ServerGame) extends ServerCommand
case class Leave(client: ServerClient) extends ServerCommand
case class Login(client: ServerClient, username: String, challenge: String, salt: String) extends ServerCommand
case class Create(client: ServerClient, username: String, timers: Long) extends ServerCommand
case class Logout(client: ServerClient) extends ServerCommand
case class RegisterGPS(client: ServerClient, long: Int, lat: Int) extends ServerCommand
case class GetGPS(username: String) extends ServerCommand

case class ServerException(msg: String) extends RuntimeException(msg)

