package ChessServer

import java.net.ServerSocket;

class Server(port: Int) {
    val serverSocket = new ServerSocket(port)

    val games: List[ServerGame] = Nil;

    def start = {
        println("Listening to port "+port+"...");
        while(true) ServerClient(this, serverSocket.accept())
    }

}



class ServerGame(val host: ServerClient, val timers: Int) {
    import logic._

    var game = new Game(timers)
    var opponent: Option[ServerClient] = None

    def join(player: Option[ServerClient]) = opponent match {
        case Some(op) =>
            throw ProtocolException("This game is already full")
        case None =>
            opponent = player
            game = game.start
    }

    def sendOrDisc(to: ServerClient, content: String) = {
        try {
            to.out.println(content)
        } catch {
            case e => 
                println("Client disconnected: "+e.getMessage)
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
