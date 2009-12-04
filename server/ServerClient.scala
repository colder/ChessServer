package ChessServer.server

import java.net._
import java.io._

import java.util.UUID

import logic._

abstract class ClientStatus;
object Annonymous extends ClientStatus;
object Logged extends ClientStatus;

import scala.collection.mutable.HashMap
import scala.actors.Actor
import scala.actors.Actor._

case class ServerClient(server: Server, sock: Socket) extends Actor {
    /* Logger */
    val log = new TerminalLogger
    log.setIP(sock.getInetAddress().toString)
    var status: ClientStatus = Annonymous
    var username: String = ""
    var userid: Int = -1

    private var games = new HashMap[String, ServerGame]()

    private val out =  new PrintWriter(new OutputStreamWriter(sock.getOutputStream()))

    private val salt: String = {
        UUID.randomUUID.toString
    }

    private val listener = new Thread() {
        private val in = new BufferedReader(new InputStreamReader(sock.getInputStream()))

        override def run = {
            log.info("Client connected!");

            send(<hello salt={ salt } />)
            sock.setSoTimeout(1000*60*20)

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
                    case e: java.net.SocketTimeoutException =>
                        log.err("Client timeout")
                        continue = false
                    case e =>
                        log.err(e.toString)
                        continue = false;
                }
            }

            server !? Leave(ServerClient.this)

            ServerClient.this ! CloseClient

            in.close
            log.info("Client disconnected!");
        }
    }

    private def userlog = if (status != Annonymous) "["+username+"] " else "[@] "

    private def parseLine(line: String): Boolean = try {
        import scala.xml._

        log.in(userlog+line);
        val data = XML.loadString(line)

        data match {
            case <auth>{ a }</auth> =>
                a match {
                    case Elem(_, "login", attr, _) =>
                        if (attr.get("challenge") != None && attr.get("username") != None) {
                            if (status == Annonymous) {
                                server !? Login(this, attr("username").toString, attr("challenge").toString, salt) match {
                                    case Success(id: Int) =>
                                        status = Logged
                                        username = attr("username").toString
                                        userid = id
                                        sendAuthAck
                                    case Failure(msg) =>
                                        sendAuthNack(msg);
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
                            server !? Logout(this)
                            userid = -1
                            sendAuthAck
                        } else {
                            sendAuthAck
                        }
                    case _ =>
                        sendAuthNack("Unknown games command");

                }
                true


            case Elem(_, "chess", attr, _, g) if status == Logged && attr.get("username") != None =>
                val username = attr("username").toString
                (games.get(username), g) match {
                    // Create doesn't require any game
                    case (_, Elem(_, "create", attr, _)) if (attr.get("timers") != None) =>
                        server !? Create(this, username, attr("timers").toString.toLong) match {
                            case Success((game: ServerGame, opp: ServerClient)) =>
                                opp.send(<chess username={this.username}><invite timers={ attr("timers").toString } /></chess>)
                                games(opp.username) = game
                                sendChessAck(username)
                            case Failure(msg) =>
                                sendChessNack(username, msg);
                        }
                    case (Some(game), Elem(_, "inviteaccept", _, _))=>
                        if (game.host != this) {
                            (game !? InviteAccept) match {
                                case Success(g) =>
                                    sendChessAck(username)
                                case Failure(msg) =>
                                    sendChessNack(username, "Failed to accept invitation: "+msg)
                            }
                        } else {
                            sendChessNack(username, "You cannot accept your own invitation")
                        }

                    case (Some(game), Elem(_, "invitedecline", _, _)) =>
                        if (game.host != this) {
                            (game !? InviteDecline) match {
                                case Success(_) =>
                                    sendChessAck(username)
                                    server ! GameEnd(game)
                                case Failure(msg) =>
                                    sendChessNack(username, "Failed to cancel invitation: "+msg)
                            }
                        } else {
                            sendChessNack(username, "You cannot decline your own invitation")
                        }

                    case (Some(game), Elem(_, "move", attr, _)) if (attr.get("from") != None && attr.get("to") != None) =>
                        game ! Move(this, new Position(attr("from").toString), new Position(attr("to").toString))

                    case (Some(game), Elem(_, "movepromote", attr, _)) if (attr.get("from") != None && attr.get("to") != None && attr.get("promotion") != None) =>
                        def parsePromotion(str: String): Result[_ <: PieceType] = str.toUpperCase match {
                            case "Q" => Success(Queen)
                            case "R" => Success(Rook)
                            case "N" => Success(Knight)
                            case "B" => Success(Bishop)
                            case _ => Failure("Invalid promotion type")
                        }

                        parsePromotion(attr("promotion").toString) match {
                            case Success(pt) =>
                                game ! MovePromote(this,
                                                    new Position(attr("from").toString),
                                                    new Position(attr("to").toString),
                                                    pt)
                            case Failure(msg) =>
                                sendChessNack(username, msg);
                        }
                    case (Some(game), Elem(_, "resign", _, _)) =>
                            game ! Resign(this)
                            games -= username
                    case (Some(game), Elem(_, "drawask", _, _)) =>
                            game ! DrawAsk(this);
                    case (Some(game), Elem(_, "drawaccept", _, _)) =>
                            game ! DrawAccept(this);
                    case (Some(game), Elem(_, "drawdecline", _, _)) =>
                            game ! DrawDecline(this);
                    case (Some(game), Elem(_, "timers", _, _)) =>
                            game ! Timers(this);
                    case (Some(game), _) =>
                        sendChessNack(username, "Unknown  or invalid chess command. Maybe the parameters?");
                    case (None, _) =>
                        sendChessNack(username, "Game not found");
                }
                true
            case Elem(_, "gps", _, _, data) if status == Logged =>
                data match {
                    case Elem(_, "register", attr, _) if attr.get("long") != None && attr.get("lat") != None =>
                        server ! RegisterGPS(this, attr("long").toString.toInt, attr("lat").toString.toInt)
                        sendGPSAck

                    case Elem(_, "get", attr, _) if attr.get("username") != None =>
                        val username = attr("username").toString
                        server !? GetGPS(username) match {
                            case Some(pos: GPSPosition) =>
                                val logged = server !? GetUser(username) match {
                                    case Some(x) => "yes"
                                    case None => "no"
                                }
                                send(<gps><position username={username} long={pos.long.toString} lat={pos.lat.toString} logged={ logged } /></gps>)
                            case None =>
                                send(<gps><position username={username} long="N/A" lat="N/A" logged="no" /></gps>)
                        }

                    case _ =>
                        sendGPSNack("Invalid gps command")
                }
                true
            case Elem(_, "chat", attr, _, data) if status == Logged && (attr.get("username") != None) =>
                val username = attr("username").toString;
                server !? GetUser(username) match {
                    case Some(u: ServerClient) if !(this.username equals username) =>
                        data match {
                            case Elem(_, "msg", _, _, data) =>
                                u.send(<chat username={ this.username }><msg>{ data.toString }</msg></chat>);
                                sendChatAck(u.username)
                                server ! LogChatMessage(this, u, data.toString)
                            case Elem(_, "msg", _, _) =>
                                u.send(<chat username={ this.username }><msg></msg></chat>);
                                sendChatAck(u.username)
                                server ! LogChatMessage(this, u, data.toString)
                            case _ =>
                                sendChatNack(u.username, "Invalid chat command")
                        }
                    case Some(u: ServerClient) =>
                        sendChatNack(u.username, "Cannot send messages to yourself")
                    case None =>
                        sendChatNack(username, "Username '"+username+"' not found")
                }
                true
            case <quit /> =>
                status = Annonymous
                server !? Logout(this)
                false
            case Elem(_, label, attr, _, data @ _*) if status == Logged && attr.get("username") != None =>
                val username = attr("username").toString;
                server !? GetUser(username) match {
                    case Some(u: ServerClient) if !(this.username equals username) =>
                        u.send("<"+label+" username=\""+this.username+"\">"+data.mkString+"</"+label+">");
                        send("<"+label+" username=\""+username+"\"><ack /></"+label+">");
                    case Some(u: ServerClient) =>
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
        case e: org.xml.sax.SAXParseException =>
            sendNack("Invalid xml message: "+e.getMessage)
            true
    }

    private def sendGPSNack(msg: String) = send(<gps><nack msg={ msg } /></gps>)
    private def sendGPSAck = send(<gps><ack /></gps>)

    private def sendAuthNack(msg: String) = send(<auth><nack msg={ msg } /></auth>)
    private def sendAuthAck = send(<auth><ack /></auth>)

    private def sendChatNack(username: String, msg: String) = send(<chat username={ username }><nack msg={ msg } /></chat>)
    private def sendChatAck(username: String) = send(<chat username={ username }><ack /></chat>)

    private def sendChessNack(username: String, msg: String) = send(<chess username={ username }><nack msg={ msg } /></chess>)
    private def sendChessAck(username: String) = send(<chess username={ username }><ack /></chess>);

    private def sendNack(msg: String) = send(<nack msg={ msg } />)

    private def send(msg: xml.Node): Unit = send(msg.toString)

    private def send(msg: String): Unit = {
        log.out(userlog+msg)
        out.println(msg)
        out.flush
    }

    def act() {
        var continue = true
        while(continue) {
            receive {
                case OnGameInvite(game) =>
                    games(game.host.username) = game

                case OnGameEnd(game) =>
                    if (game.host == this) {
                        games -= game.opponent.username
                    } else {
                        games -= game.host.username
                    }

                case CloseClient =>
                    continue = false

                case Send(msg) =>
                    send(msg)

                case SendChessAck(username) =>
                    sendChessAck(username)

                case SendChessNack(username, msg) =>
                    sendChessNack(username, msg)

            }
        }

        out.close
    }

    listener.start

    start
}

abstract class ServerClientCommand;
case class SendChessAck(username: String) extends ServerClientCommand
case class SendChessNack(username: String, msg: String) extends ServerClientCommand
case class Send(msg: xml.Node) extends ServerClientCommand
case class OnGameInvite(game: ServerGame) extends ServerClientCommand
case class OnGameEnd(game: ServerGame) extends ServerClientCommand
case object CloseClient extends ServerClientCommand
