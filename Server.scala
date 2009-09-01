package ChessServer

import java.net.ServerSocket;
import scala.collection.mutable.HashMap

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    var games = new HashMap[String, ServerGame]()

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

    def create(client: ServerClient, timers: Long): Option[ServerGame] = {
        if (games contains client.username) {
            client.send(<games><nack msg="You're already playing a game!" /></games>)
            None
        } else {
            val game = ServerGame(client, timers)
            games(client.username) = game
            client.send(<games><ack /></games>)
            Some(game)
        }
    }

    def create(client: ServerClient, timers: Long): Option[ServerGame] = {
        if (games contains client.username) {
            client.send(<games><nack msg="You're already playing a game!" /></games>)
            None
        } else {
            val game = ServerGame(client, timers)
            games(client.username) = game
            client.send(<games><ack /></games>)
            Some(game)
        }
    }

    def join(client: ServerClient, host: String, timers: Long): Option[ServerGame] = {
        if (games contains client.username) {
            client.send(<games><nack msg="You're already playing a game!" /></games>)
            None
        } else {
            games.get(host) match {
                case Some(g) =>
                    if (g.started) {
                        client.send(<games><nack msg="Game not found!" /></games>)
                        None
                    } else {
                        g.join(client)
                        Some(g)
                    }

                case None =>
                    client.send(<games><nack msg="Game not found!" /></games>)
                    None
            }
        }
    }

    def listGames(client: ServerClient) = {
        if (client.status == Annonymous) {
            client.send(<games><nack msg="Please login first" /></games>)
            None
        } else {
            client.send("<games>"+{ games.values.map { g => "<game host=\""+g.host+"\" timers=\""+g.timers+"\" />" }.mkString }+"</games>")
        }
    }

}



case class ServerGame(val host: ServerClient, val timers: Long) {
    import logic._

    var game = new Game(timers)
    var opponent: Option[ServerClient] = None

    def started = opponent != None

    def join(player: ServerClient) = opponent match {
        case Some(op) =>
            throw ProtocolException("This game is already full")
        case None =>
            opponent = Some(player)
            game = game.start
    }

    def sendOrDisc(to: ServerClient, content: String) = {
        try {
            to.send(content)
        } catch {
            case e =>
                println("Client disconnected: "+e.getMessage)
                // TODO disconnect the user
        }
    }

    def replyNack(player: ServerClient, reason: String) = {
        sendOrDisc(player, <game><nack msg={ reason } /></game>.toString)
    }

    def dispatch(player: ServerClient, msg: String) = {
        // Send ack to the sender
        sendOrDisc(player, <game><ack /></game>.toString)
        // transmit the message to the other
        if (player == host) {
            opponent match {
                case Some(op) => sendOrDisc(op, msg)
                case None => println("Trying to send to an innexistent opponent!")
            }
        } else {
            sendOrDisc(host, msg)
        }
    }

    def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node) = {
        try {
            checkGameConditions(player)
            action
            dispatch(player, dispatchMsg.toString)
        } catch {
            case e => replyNack(player, e.getMessage);
        }
    }

    def move(player: ServerClient, from: Position, to: Position) =
        op(player, game = game.move(from, to), <game><move from={ from.algNotation } to={ to.algNotation } /></game>)

    def moveAndPromote(player: ServerClient, from: Position, to: Position, promotion: PieceType) =
        op(player, game = game.move(from, to), <game><move from={ from.algNotation } to={ to.algNotation } promotion={ promotion.ab } /></game>)

    def drawAsk(player: ServerClient) =
        op(player, game = game.drawAsk, <game><drawAsk /></game>)

    def drawAccept(player: ServerClient) =
        op(player, game = game.drawAccept, <game><drawAccept /></game>)

    def drawDecline(player: ServerClient) =
        op(player, game = game.drawDecline, <game><drawDecline /></game>)


    def checkGameConditions(player: ServerClient) = {
        if (player == host && game.turn != White || player != host && game.turn != Black) {
            throw ProtocolException("Wait your turn!");
        } else if (opponent == None) {
            throw ProtocolException("Wait for your opponent!");
        }
    }
}

case class ProtocolException(msg: String) extends RuntimeException(msg)
