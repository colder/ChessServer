package ChessServer.server

import java.net._
import java.io._

import java.util.UUID

abstract class ClientStatus;
object Annonymous extends ClientStatus;
object Logged extends ClientStatus;

import scala.collection.mutable.HashMap
case class ServerClient(server: Server, sock: Socket) extends Thread {
    var status: ClientStatus = Annonymous
    var username: String = ""
    var games = new HashMap[String, ServerGame]()

    private val in = new BufferedReader(new InputStreamReader(sock.getInputStream()))
    private val out =  new PrintWriter(new OutputStreamWriter(sock.getOutputStream()))

    val salt: String = {
        UUID.randomUUID.toString
    }

    override def run = {
        println("Client connected!");

        send(<hello salt={ salt } />)

        var continue = true;
        while (continue) {
            try {
                val line = in.readLine;
                if (line == null) {
                    continue = false;
                } else {
                    continue = parseLine(line);
                }
            } catch {
                case e =>
                    println("! "+e)
                    continue = false;
            }
        }

        server.leave(this)

        println("Client disconnected!");
    }

    def onGameEnd(game: ServerGame) = {
        //_game = None
    }

    def userlog = if (status != Annonymous) "["+username+"] " else "[@] "

    def onJoin(game: ServerGame, player: ServerClient) = {
        games(player.username) = game
    }

    def parseLine(line: String): Boolean = try {
        import scala.xml._

        println("< "+userlog+line);
        val data = XML.loadString(line)

        data match {
            case <auth>{ a }</auth> =>
                a match {
                    case Elem(_, "login", attr, _) =>
                        if (attr.get("challenge") != None && attr.get("username") != None) {
                            if (status == Annonymous) {
                                if (server.login(this, attr("username").toString, attr("challenge").toString, salt)) {
                                    status = Logged
                                    username = attr("username").toString
                                    sendAck
                                } else {
                                    sendNack("Invalid user/pass");
                                }
                            } else {
                                sendNack("You're already logged");
                            }
                        } else {
                            sendNack("Invalid auth.login command");
                        }
                    case Elem(_, "logout", attr, _) =>
                        if (status == Logged) {
                            status = Annonymous
                            server.logout(this)
                            sendAck
                        }
                    case _ =>
                        sendNack("Unknown games command");

                }
                true


            case Elem(_, "chess", attr, _, g) if attr.get("username") == None => g match {
                // <chess> commands that do not require any username paramt
                case Elem(_, "create", attr, _) =>
                    if (attr.get("timers") != None) {
                        try {
                            server.create(this, attr("timers").toString.toLong)
                            sendAck
                        } catch {
                            case se: ServerException =>
                                sendNack("Failed to create: "+se.getMessage)
                        }
                    } else {
                        sendNack("Invalid chess.create command");
                    }
                case Elem(_, "join", attr, _) =>
                    if (attr.get("username") != None && attr.get("timers") != None) {
                        try {
                            val g = server.join(this, attr("username").toString, attr("timers").toString.toLong)
                            games(g.host.username) = g
                            sendAck
                        } catch {
                            case se: ServerException =>
                                sendNack("Failed to join: "+se.getMessage)
                        }
                    } else {
                        sendNack("Invalid chess.create command");
                    }
                case Elem(_, "list", attr, _) =>
                    send("<chess>"+{ server.freeGames(this).map { g => "<game username=\""+g.host.username+"\" timers=\""+g.ts+"\" />" }.mkString }+"</chess>")

                case _ =>
                    sendNack("Invalid chess command, maybe you forgot the username parameter?");
            }
            true

            case Elem(_, "chess", attr, _, g) if attr.get("username") != None =>
                val game = {
                    games.get(attr("username").toString) match {
                        case Some(g) => g
                        case None =>
                            throw new ServerException("Game not found!");
                    }
                }

                import logic._
                g match {
                    case Elem(_, "move", attr, _) =>
                        if (attr.get("from") != None && attr.get("to") != None) {
                            game.move(this,
                                        new Position(attr("from").toString),
                                        new Position(attr("to").toString))
                        } else {
                            sendNack("Invalid chess.move command");
                        }
                    case Elem(_, "movepromote", attr, _) =>
                        def parsePromotion(str: String): PieceType = str.toUpperCase match {
                            case "Q" => Queen
                            case "R" => Rook
                            case "N" => Knight
                            case "B" => Bishop
                            case _ =>
                                throw new ServerException("Invalid promotion type");
                        }
                        if (attr.get("from") != None && attr.get("to") != None && attr.get("promotion") != None) {
                            try {
                                game.moveAndPromote(this,
                                                        new Position(attr("from").toString),
                                                        new Position(attr("to").toString),
                                                        parsePromotion(attr("promotion").toString))
                            } catch {
                                case e: ServerException =>
                                    sendNack(e.getMessage);
                            }
                        } else {
                            sendNack("Invalid chess.movepromote command");
                        }
                    case Elem(_, "resign", _, _) =>
                            game.resign(this)
                    case Elem(_, "drawask", _, _) =>
                            game.drawAsk(this);
                    case Elem(_, "drawaccept", _, _) =>
                            game.drawAccept(this);
                    case Elem(_, "drawdecline", _, _) =>
                            game.drawDecline(this);
                    case Elem(_, "timers", _, _) =>
                            game.timers(this);
                    case _ =>
                        sendNack("Unknown chess command");
                }
                true
            case <quit /> =>
                status = Annonymous
                server.logout(this)
                sendAck
                false
            case <chat>{ a }</chat> =>
                a match {
                    case Elem(_, "msg", attr, _, data) if status != Annonymous=>
                        if (attr.get("username") != None) {
                            val username = attr("username").toString;
                            server.users.get(username) match {
                                case Some(u) if !(this.username equals username) =>
                                    u.send(<chat><msg username={ this.username } >{ data.toString }</msg></chat>);
                                    sendAck
                                case Some(u) =>
                                    sendNack("Cannot send messages to yourself")
                                case None =>
                                    sendNack("Username \""+username+"\" not found")
                            }
                        }
                    case _ if status != Annonymous=>
                        sendNack("Unknown game command");
                    case _ =>
                        sendNack("You need to be at least logged")
                }
                true
            case _ =>
                sendNack("woot?")
                true
        }
    } catch {
        case e => sendNack(e.getMessage); true
    }

    def sendNack(msg: String) = send(<nack msg={ msg } />);
    def sendAck = send(<ack />);

    def send(msg: xml.Node): Unit = send(msg.toString)
    def send(msg: String): Unit = {
        println("> "+userlog+msg)
        out.println(msg)
        out.flush
    }

    start
}
