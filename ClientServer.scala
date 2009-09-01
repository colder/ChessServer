package ChessServer

import java.net._
import java.io._

abstract class ClientStatus;
case object Annonymous extends ClientStatus;
case object Logged extends ClientStatus;
case object Playing extends ClientStatus;

case class ServerClient(server: Server, sock: Socket) extends Thread {
    var clientState: ClientStatus = Annonymous;

    override def run = {
        println("Client connected!");
        val br = new BufferedReader(new InputStreamReader(sock.getInputStream()))
        val pw =  new PrintWriter(new OutputStreamWriter(sock.getOutputStream()))

        var continue = true;
        while (continue) {
            try {
                val line = br.readLine;
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
            case <auth>{ a }</auth> => a match {
                case Elem(_, "login", attr, _) =>
                    if (attr.get("password") != None && attr.get("username") != None) {
                        //TODO
                    } else {
                        throw new ProtocolException("Invalid auth.login command");
                    }
                case Elem(_, "logout", attr, _) =>
                        //TODO

                case _ =>
                    throw new ProtocolException("Unknown games command");

            }
            case <games>{ g }</games> => g match {
                case Elem(_, "create", attr, _) =>
                    if (attr.get("timers") != None) {
                        //TODO
                    } else {
                        throw new ProtocolException("Invalid games.create command");
                    }
                case Elem(_, "list", attr, _) =>
                        //TODO

                case _ =>
                    throw new ProtocolException("Unknown games command");

            }
            case <game>{ g }</game> => g match {
                case Elem(_, "move", attr, _) => 
                    if (attr.get("from") != None && attr.get("to") != None) {
                        //TODO
                    } else {
                        throw new ProtocolException("Invalid game.move command");
                    }
                case Elem(_, "movepromote", attr, _) =>
                    if (attr.get("from") != None && attr.get("to") != None && attr.get("promotion") != None) {
                        //TODO
                    } else {
                        throw new ProtocolException("Invalid game.movepromote command");
                    }
                case Elem(_, "resign", _, _) =>
                    // TODO
                case Elem(_, "drawask", _, _) =>
                    // TODO
                case Elem(_, "drawaccept", _, _) =>
                    // TODO
                case Elem(_, "drawdecline", _, _) =>
                    // TODO
                case Elem(_, "timers", _, _) =>
                    // TODO
                case _ =>
                    throw new ProtocolException("Unknown game command");
            }
            case _ =>
                println("ignore...");
        }
        true
    }

    start
}
