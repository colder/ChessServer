package ChessServer.server

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

    def dispatch(player: ServerClient, msg: String) = {
        player.sendAck
        // transmit the message to the other player
        if (player == host) {
            opponent match {
                case Some(otherplayer) => otherplayer.send(msg)
                case None => println("Trying to send to an innexistent opponent!")
            }
        } else {
            host.send(msg)
        }
    }

    def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node) = {
        try {
            checkGameConditions(player)
            action
            dispatch(player, dispatchMsg.toString)
        } catch {
            case e => player.sendNack(e.getMessage);
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
