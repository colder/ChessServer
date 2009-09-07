package ChessServer.server

case class ServerGame(val host: ServerClient, val ts: Long) {
    import logic._

    var game = new Game(ts)
    var opponent: Option[ServerClient] = None

    def started = opponent != None

    def join(player: ServerClient) = opponent match {
        case Some(op) =>
            throw ProtocolException("This game is already full")
        case None =>
            opponent = Some(player)
            game = game.start
            dispatch(player, <game><joined username={ player.username } /></game>.toString)
    }

    def dispatch(player: ServerClient, msg: String) = {
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
            player.sendAck
            dispatch(player, dispatchMsg.toString)
        } catch {
            case e => player.sendNack(e.getMessage);
        }
    }

    def timers(player: ServerClient) = {
        player.send(<game><timers white={ game.timers._1.toString } black={ game.timers._2.toString } /></game>)
    }

    def move(player: ServerClient, from: Position, to: Position) =
        op(player, game = game.move(from, to), <game><move from={ from.algNotation } to={ to.algNotation } /></game>)

    def moveAndPromote(player: ServerClient, from: Position, to: Position, promotion: PieceType) =
        op(player, game = game.move(from, to), <game><move from={ from.algNotation } to={ to.algNotation } promotion={ promotion.ab } /></game>)

    def resign(player: ServerClient) =
        op(player, game = game.resign, <game><resign /></game>)

    def drawAsk(player: ServerClient) =
        op(player, game = game.drawAsk, <game><drawask /></game>)

    def drawAccept(player: ServerClient) =
        op(player, game = game.drawAccept, <game><drawaccept /></game>)

    def drawDecline(player: ServerClient) =
        op(player, game = game.drawDecline, <game><drawdecline /></game>)


    def checkGameConditions(player: ServerClient) = {
        if (player == host && game.turn != White || player != host && game.turn != Black) {
            throw ProtocolException("Wait your turn!");
        } else if (opponent == None) {
            throw ProtocolException("Wait for your opponent!");
        }
    }
}

case class ProtocolException(msg: String) extends RuntimeException(msg)
