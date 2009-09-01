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
            game.start
    }

    def move(player: ServerClient, from: Position, to: Position) = {
        checkPlayerTurn(player)
    }

    def moveAndPromote(player: ServerClient, from: Position, to: Position, promotion: PieceType) = {
        checkPlayerTurn(player)
    }

    def timers(player: ServerClient) = {

    }

    def drawAsk(player: ServerClient) = {
        checkPlayerTurn(player)
        game = game.drawAccept
    }

    def drawAccept(player: ServerClient) = {
        checkPlayerTurn(player)
        game = game.drawAccept
    }

    def drawDecline(player: ServerClient) = {
        checkPlayerTurn(player)
        game = game.drawDecline
    }

    def checkPlayerTurn(player: ServerClient) = {
        if (player == host && game.turn != White || player != host && game.turn != Black) {
            throw ProtocolException("Wait your turn!");
        }
    }
}

case class ProtocolException(msg: String) extends RuntimeException(msg)
