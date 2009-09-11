package ChessServer.server

case class ServerGame(val server: Server, val host: ServerClient, val ts: Long) {
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

    def end = {
        // dispatch the game ending status to both players
        val msg: Option[xml.Node] = game.status match {
            case GameWinBlack =>
                Some(<game><end winner="black" /></game>)
            case GameWinWhite =>
                Some(<game><end winner="white" /></game>)
            case GameDraw =>
                Some(<game><draw /></game>)
            case _ =>
                None
        }

        msg match {
            case Some(m) =>
                host.send(m)
                opponent match {
                    case Some(op) => op.send(m)
                    case None =>
                }
            case None =>
        }

        server.gameEnd(this);
    }

    def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node): Boolean = {
        try {
            checkGameConditions(player)
            action
            player.sendAck
            dispatch(player, dispatchMsg.toString)
            true
        } catch {
            case e => player.sendNack(e.getMessage)
                      false
        }
    }

    def timers(player: ServerClient) = {
        player.send(<game><timers white={ game.timers._1.toString } black={ game.timers._2.toString } /></game>)
    }

    def move(player: ServerClient, from: Position, to: Position) =
        op(player, game = game.move(from, to), <game><move from={ from.algNotation } to={ to.algNotation } /></game>)

    def moveAndPromote(player: ServerClient, from: Position, to: Position, promotion: PieceType) =
        op(player, game = game.move(from, to), <game><move from={ from.algNotation } to={ to.algNotation } promotion={ promotion.ab } /></game>)

    def resign(player: ServerClient) = {
        val loser = if (player == host) White else Black
        try {
            game = game.resign(loser)
            player.sendAck
            end
        } catch {
            case e => player.sendNack(e.getMessage)
        }
    }

    def drawAsk(player: ServerClient) =
        op(player, game = game.drawAsk, <game><drawask /></game>)

    def drawAccept(player: ServerClient) =
        try {
            checkGameConditions(player)
            game = game.drawAccept
            player.sendAck
            end
            true
        } catch {
            case e => player.sendNack(e.getMessage)
                      false
        }

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
