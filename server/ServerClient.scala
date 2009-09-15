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
    var userid: Int = -1
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
                                server.login(this, attr("username").toString, attr("challenge").toString, salt) match {
                                    case Success(id) =>
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
                            server.logout(this)
                            userid = -1
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
                        server.create(this, username, attr("timers").toString.toLong) match {
                            case Success((game, opp)) =>
                                opp.send(<chess username={this.username}><invite timers={ attr("timers").toString } /></chess>)
                                games(opp.username) = game
                                sendChessAck(username)
                            case Failure(msg) =>
                                sendChessNack(username, msg);
                        }
                    // inviteaccept doesn't require any game
                    case (_, Elem(_, "inviteaccept", _, _)) =>
                        server.inviteaccept(this, username) match {
                            case Success(g) =>
                                games(username) = g
                                sendChessAck(username)
                            case Failure(msg) =>
                                sendChessNack(username, "Failed to accept invitation: "+msg)
                        }

                    // invitedecline doesn't require any game
                    case (_, Elem(_, "invitedecline", _, _)) =>
                        server.invitedecline(this, username) match {
                            case Success(g) =>
                                sendChessAck(username)
                            case Failure(msg) =>
                                sendChessNack(username, "Failed to cancel invitation: "+msg)
                        }

                    case (Some(game), Elem(_, "move", attr, _)) if (attr.get("from") != None && attr.get("to") != None) =>
                        game.move(this, new Position(attr("from").toString), new Position(attr("to").toString))

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
                                game.moveAndPromote(this,
                                                    new Position(attr("from").toString),
                                                    new Position(attr("to").toString),
                                                    pt)
                            case Failure(msg) =>
                                sendChessNack(username, msg);
                        }
                    case (Some(game), Elem(_, "resign", _, _)) =>
                            game.resign(this)
                            games -= username
                    case (Some(game), Elem(_, "drawask", _, _)) =>
                            game.drawAsk(this);
                    case (Some(game), Elem(_, "drawaccept", _, _)) =>
                            game.drawAccept(this);
                    case (Some(game), Elem(_, "drawdecline", _, _)) =>
                            game.drawDecline(this);
                    case (Some(game), Elem(_, "timers", _, _)) =>
                            game.timers(this);
                    case (Some(game), _) =>
                        sendChessNack(username, "Unknown  or invalid chess command. Maybe the parameters?");
                    case (None, _) =>
                        sendChessNack(username, "Game not found");
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
                                val logged = server.users.get(username) match {
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
                data match {
                    case Elem(_, "msg", _, _, data) =>
                        server.users.get(username) match {
                            case Some(u) if !(this.username equals username) =>
                                u.send(<chat username={ this.username }><msg>{ data.toString }</msg></chat>);
                                sendChatAck(u.username)
                                server.logChatMessage(this, u, data.toString)
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
    }

    def sendGPSNack(msg: String) = send(<gps><nack msg={ msg } /></gps>)
    def sendGPSAck = send(<gps><ack /></gps>)

    def sendAuthNack(msg: String) = send(<auth><nack msg={ msg } /></auth>)
    def sendAuthAck = send(<auth><ack /></auth>)

    def sendChatNack(username: String, msg: String) = send(<chat username={ username }><nack msg={ msg } /></chat>)
    def sendChatAck(username: String) = send(<chat username={ username }><ack /></chat>)

    def sendChessNack(username: String, msg: String) = send(<chess username={ username }><nack msg={ msg } /></chess>)
    def sendChessAck(username: String) = send(<chess username={ username }><ack /></chess>);

    def sendNack(msg: String) = send(<nack msg={ msg } />)

    def send(msg: xml.Node): Unit = send(msg.toString)
    def send(msg: String): Unit = {
        println("> "+userlog+msg)
        out.println(msg)
        out.flush
    }

    start
}
