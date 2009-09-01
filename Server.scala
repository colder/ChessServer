package ChessServer

import java.net._
import java.io._

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

}

case class ServerClient(server: Server, sock: Socket) extends Thread {

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
            case <action>{ a }</action> => a match {
                case Elem(_, "move", _, _) =>
                    println(a);
                case _ =>
                    println("Uoup? "+a);

            }
            case _ =>
                println("ignore...");
        }
        true
    }

    start
}
