package ChessServer.server

import java.net._
import java.io._

import java.util.UUID

abstract class ClientStatus;
object Annonymous extends ClientStatus;
object Logged extends ClientStatus;
object Playing extends ClientStatus;

case class ServerClient(server: Server, sock: Socket) extends Thread {
    var status: ClientStatus = Annonymous
    var username: String = ""
    var _game: Option[ServerGame] = None
    def game: ServerGame = {
        _game getOrElse (throw new ServerException("Invalid state, there should be a game at this point!"))
    }

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

    def onGameEnd = {
        status = Logged
        _game = None
    }

    def userlog = if (status != Annonymous) "["+username+"] " else "[@] "

    def parseLine(line: String): Boolean = {
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
                        } else if (status == Playing) {
                            sendNack("Leave your game first!");
                        }
                    case _ =>
                        sendNack("Unknown games command");

                }
                true

            case <games>{ g }</games> =>
                if (status == Logged) {
                    g match {
                        case Elem(_, "create", attr, _) =>
                            if (attr.get("timers") != None) {
                                try {
                                    _game = Some(server.create(this, attr("timers").toString.toLong))
                                    status = Playing
                                    sendAck
                                } catch {
                                    case se: ServerException =>
                                        sendNack("Failed to create: "+se.getMessage)
                                }
                            } else {
                                sendNack("Invalid games.create command");
                            }
                        case Elem(_, "join", attr, _) =>
                            if (attr.get("username") != None) {
                                try {
                                    _game = Some(server.join(this, attr("username").toString))
                                    status = Playing
                                    sendAck
                                } catch {
                                    case se: ServerException =>
                                        sendNack("Failed to join: "+se.getMessage)
                                }
                            } else {
                                sendNack("Invalid games.create command");
                            }
                        case Elem(_, "list", attr, _) =>
                            send("<games>"+{ server.freeGames.map { g => "<game username=\""+g.host.username+"\" timers=\""+g.ts+"\" />" }.mkString }+"</games>")

                        case _ =>
                            sendNack("Unknown games command");

                    }
                } else {
                    if (status == Annonymous) {
                        sendNack("You need to be logged")
                    } else {
                        sendNack("Leave your game fisrt")
                    }
                }
                true

            case <game>{ g }</game> =>
                import logic._
                g match {
                    case Elem(_, "move", attr, _) if status == Playing =>
                        if (attr.get("from") != None && attr.get("to") != None) {
                            game.move(this,
                                        new Position(attr("from").toString),
                                        new Position(attr("to").toString))
                        } else {
                            sendNack("Invalid game.move command");
                        }
                    case Elem(_, "movepromote", attr, _) if status == Playing =>
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
                            sendNack("Invalid game.movepromote command");
                        }
                    case Elem(_, "resign", _, _) if status == Playing =>
                            game.resign(this)
                    case Elem(_, "drawask", _, _) if status == Playing =>
                            game.drawAsk(this);
                    case Elem(_, "drawaccept", _, _) if status == Playing =>
                            game.drawAccept(this);
                    case Elem(_, "drawdecline", _, _) if status == Playing =>
                            game.drawDecline(this);
                    case Elem(_, "timers", _, _) if status == Playing =>
                            game.timers(this);
                    case _ if status == Playing =>
                        sendNack("Unknown game command");
                    case _ =>
                        sendNack("You need to be playing")
                }
                true
            case <quit /> =>
                if (_game != None) {
                    game.resign(this)
                }
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
