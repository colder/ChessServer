package ChessServer.server

import java.net._
import java.io._

import java.util.UUID

import logic._

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
                                    sendAuthAck
                                } else {
                                    sendAuthNack("Invalid user/pass");
                                }
                            } else {
                                sendAuthNack("You're already logged");
                            }
                        } else {
                            sendAuthNack("Invalid auth.login command");
                        }
                    case Elem(_, "logout", attr, _) =>
                        if (status == Logged) {
                            status = Annonymous
                            server.logout(this)
                            sendAuthAck
                        }
                    case _ =>
                        sendAuthNack("Unknown games command");

                }
                true


            // <chess> commands that do not require any username param
            case Elem(_, "chess", attr, _, g) if status == Logged && attr.get("username") == None => g match {
                case Elem(_, "create", attr, _) =>
                    if (attr.get("timers") != None) {
                        try {
                            server.create(this, attr("timers").toString.toLong)
                            sendChessAck
                        } catch {
                            case se: ServerException =>
                                sendChessNack("Failed to create: "+se.getMessage)
                        }
                    } else {
                        sendChessNack("Invalid chess.create command");
                    }
                case Elem(_, "join", attr, _) =>
                    if (attr.get("username") != None && attr.get("timers") != None) {
                        try {
                            val g = server.join(this, attr("username").toString, attr("timers").toString.toLong)
                            games(g.host.username) = g
                            sendChessAck
                        } catch {
                            case se: ServerException =>
                                sendChessNack("Failed to join: "+se.getMessage)
                        }
                    } else {
                        sendChessNack("Invalid chess.create command");
                    }
                case Elem(_, "list", attr, _) =>
                    send("<chess>"+{ server.freeGames(this).map { g => "<game username=\""+g.host.username+"\" timers=\""+g.ts+"\" />" }.mkString }+"</chess>")

                case _ =>
                    sendChessNack("Invalid chess command, maybe you forgot the username parameter?");
            }
            true

            case Elem(_, "chess", attr, _, g) if status == Logged && attr.get("username") != None =>
                val username = attr("username").toString
                def game = {
                    games.get(username) match {
                        case Some(g) => g
                        case None =>
                            throw new ServerException("Game not found!");
                    }
                }

                g match {
                    case Elem(_, "move", attr, _) =>
                        if (attr.get("from") != None && attr.get("to") != None) {
                            game.move(this,
                                        new Position(attr("from").toString),
                                        new Position(attr("to").toString))
                        } else {
                            sendChessNack(username, "Invalid chess.move command");
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
                                    sendChessNack(username, e.getMessage);
                            }
                        } else {
                            sendChessNack(username, "Invalid chess.movepromote command");
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
                    case Elem(_, "invite", attr, _) if attr.get("timers") != None =>
                        server.users.get(username) match {
                            case Some(u) if !(this.username equals username) =>
                                u.send(<chess username={ this.username }><invite timers={ attr("timers").toString } /></chess>);
                                sendChessAck(u.username)
                            case Some(u) =>
                                sendChessNack(u.username, "Cannot send invitations to yourself")
                            case None =>
                                sendChessNack(username, "Username '"+username+"' not found")
                        }
                    case _ =>
                        sendChessNack(username, "Unknown chess command");
                }
                true
            case Elem(_, "gps", _, _, data) if status == Logged =>
                data match {
                    case Elem(_, "register", attr, _) if attr.get("long") != None && attr.get("lat") != None =>
                        server.registerGPS(this, attr("long").toString.toInt, attr("lat").toString.toInt)
                        sendGPSAck

                    case Elem(_, "get", attr, _) if attr.get("username") != None =>
                        val username = attr("username").toString
                        server.getGPS(username) match {
                            case Some(pos) =>
                                send(<gps><position username={username} long={pos.long.toString} lat={pos.lat.toString} /></gps>)
                            case None =>
                                send(<gps><position username={username} long="N/A" lat="N/A" /></gps>)
                        }

                    case _ =>
                        sendGPSNack("Invalid gps command")
                }
                true
            case Elem(_, "chat", attr, _, data) if status == Logged && (attr.get("username") != None) =>
                val username = attr("username").toString;
                data match {
                    case Elem(_, "msg", _, _, data) =>
                        server.users.get(username) match {
                            case Some(u) if !(this.username equals username) =>
                                u.send(<chat username={ this.username }><msg>{ data.toString }</msg></chat>);
                                sendChatAck(u.username)
                            case Some(u) =>
                                sendChatNack(u.username, "Cannot send messages to yourself")
                            case None =>
                                sendChatNack(username, "Username '"+username+"' not found")
                        }
                }
                true
            case <quit /> =>
                status = Annonymous
                server.logout(this)
                false
            case Elem(_, label, attr, _, data) if status == Logged && attr.get("username") != None =>
                val username = attr("username").toString;
                server.users.get(username) match {
                    case Some(u) if !(this.username equals username) =>
                        u.send("<"+label+" username=\""+username+"\">"+data.toString+"</"+label+">");
                        send("<"+label+" username=\""+username+"\"><ack /></"+label+">");
                    case Some(u) =>
                        send("<"+label+" username=\""+username+"\"><nack msg=\"Can't send to yourself\" /></"+label+">");
                    case None =>
                        send("<"+label+" username=\""+username+"\"><nack msg=\"Username not found\" /></"+label+">");
                }
                true
            case _ if status == Logged =>
                sendNack("Woot?")
                true
            case _ =>
                sendAuthNack("Login first")
                true
        }
    } catch {
        case e => sendNack("Ooups! "+e.getMessage); true
    }

    def sendGPSNack(msg: String) = send(<gps><nack msg={ msg } /></gps>)
    def sendGPSAck = send(<gps><ack /></gps>)

    def sendAuthNack(msg: String) = send(<auth><nack msg={ msg } /></auth>)
    def sendAuthAck = send(<auth><ack /></auth>)

    def sendChatNack(username: String, msg: String) = send(<chat username={ username }><nack msg={ msg } /></chat>)
    def sendChatAck(username: String) = send(<chat username={ username }><ack /></chat>)

    def sendChessNack(username: String, msg: String) = send(<chess username={ username }><nack msg={ msg } /></chess>)
    def sendChessAck(username: String) = send(<chess username={ username }><ack /></chess>);
    def sendChessNack(msg: String) = send(<chess><nack msg={ msg } /></chess>)
    def sendChessAck = send(<chess><ack /></chess>);

    def sendNack(msg: String) = send(<nack msg={ msg } />)

    def send(msg: xml.Node): Unit = send(msg.toString)
    def send(msg: String): Unit = {
        println("> "+userlog+msg)
        out.println(msg)
        out.flush
    }

    start
}
