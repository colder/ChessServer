package ChessServer.server

import java.net._
import java.io._

abstract class ClientStatus;
case object Annonymous extends ClientStatus;
case object Logged extends ClientStatus;
case object Playing extends ClientStatus;

case class ServerClient(server: Server, sock: Socket) extends Thread {
    var status: ClientStatus = Annonymous
    var username: String = ""
    var _game: Option[ServerGame] = None
    def game: ServerGame = {
        _game getOrElse (throw new ServerException("Invalid state, there should be a game at this point!"))
    }

    private val in = new BufferedReader(new InputStreamReader(sock.getInputStream()))
    private val out =  new PrintWriter(new OutputStreamWriter(sock.getOutputStream()))

    override def run = {
        println("Client connected!");

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
    }

    def parseLine(line: String): Boolean = {
        import scala.xml._

        println("< "+line);
        val data = XML.loadString(line)

        data match {
            case <auth>{ a }</auth> =>
                a match {
                    case Elem(_, "login", attr, _) =>
                        if (attr.get("password") != None && attr.get("username") != None) {
                            if (status == Annonymous) {
                                if (server.auth(attr("username").toString, attr("password").toString, "")) {
                                    status = Logged
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
                            sendAck
                        } else if (status == Playing) {
                            sendNack("Resign from your game first!");
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
                            if (attr.get("host") != None) {
                                try {
                                    server.join(this, attr("host").toString)
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
                            send("<games>"+{ server.games.values.map { g => "<game host=\""+g.host+"\" timers=\""+g.timers+"\" />" }.mkString }+"</games>")

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
                if (status == Playing) {
                    g match {
                        case Elem(_, "move", attr, _) => 
                            if (attr.get("from") != None && attr.get("to") != None) {
                                game.move(this,
                                            new Position(attr.get("from").toString),
                                            new Position(attr.get("to").toString))
                            } else {
                                sendNack("Invalid game.move command");
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
                                                            new Position(attr.get("from").toString),
                                                            new Position(attr.get("to").toString),
                                                            parsePromotion(attr.get("promotion").toString))
                                } catch {
                                    case e: ServerException =>
                                        sendNack(e.getMessage);
                                }
                            } else {
                                sendNack("Invalid game.movepromote command");
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
                                game.timers;
                        case _ =>
                            sendNack("Unknown game command");
                    }
                } else {
                    sendNack("You need to be playing")
                }
                true
            case <quit /> =>
                false
                // TODO: resign

            case _ =>
                true
        }
    }

    def sendNack(msg: String) = send(<nack msg={ msg } />);
    def sendAck = send(<ack />);

    def send(msg: xml.Node): Unit = out.println(msg.toString)
    def send(msg: String): Unit = out.println(msg)

    start
}
