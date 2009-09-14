package ChessServer.server

class ServerGame(val server: Server, val host: ServerClient, val ts: Long) {
    import logic._

    var game = new Game(ts)
    var opponent: Option[ServerClient] = None

    def started = opponent != None

    def join(player: ServerClient) = opponent match {
        case Some(op) =>
            throw ProtocolException("This game is already full")
        case None =>
            opponent = Some(player)
            host.onJoin(this, player)
            game = game.start
            dispatch(player, <chess><join username={ player.username } timers={ ts.toString } /></chess>)
    }

    def otherplayer(player: ServerClient) = {
        if (player == host) {
            opponent match {
                case Some(otherplayer) => otherplayer
                case None => throw new ProtocolException("No opponent, can't dispatch message");
            }
        } else {
            host
        }
    }

    def dispatch(player: ServerClient, msg: xml.Node) = otherplayer(player).send(msg.toString)

    def end = {
        // dispatch the game ending status to both players
        val msg: Option[String => xml.Node] = game.status match {
            case GameWinBlack =>
                Some(from => <chess username={ from }><end winner="black" /></chess>)
            case GameWinWhite =>
                Some(from => <chess username={ from }><end winner="white" /></chess>)
            case GameDraw =>
                Some(from => <chess username={ from }><draw /></chess>)
            case _ =>
                None
        }

        msg match {
            case Some(m) => opponent match {
                case Some(op) =>
                    host.send(m(op.username))
                    op.send(m(host.username))
                case None =>
                    host.send("")
            }
            case None =>
        }

        server.gameEnd(this);
    }

    def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node): Boolean =
        op(player, action, dispatchMsg, true)

    def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node, checkConditions: Boolean): Boolean = {
        try {
            if (checkConditions) checkGameConditions(player)
            action
            player.send(<chess username={ otherplayer(player).username }><ack /></chess>)

            // Check the status of the game
            game.status match {
                case _ : GameEnded =>
                    end
                case _ =>
                    dispatch(player, dispatchMsg)
            }
            true
        } catch {
            case e => player.sendChessNack(otherplayer(player).username, e.getMessage)
                      false
        }
    }

    def timers(player: ServerClient) = {
        player.send(<chess username={ otherplayer(player).username }><timers white={ game.timers._1.toString } black={ game.timers._2.toString } /></chess>)
    }

    def move(player: ServerClient, from: Position, to: Position) =
        op(player, game = game.move(from, to), <chess username={ player.username }><move from={ from.algNotation } to={ to.algNotation } /></chess>)

    def moveAndPromote(player: ServerClient, from: Position, to: Position, promotion: PieceType) =
        op(player, game = game.move(from, to), <chess username={ player.username }><move from={ from.algNotation } to={ to.algNotation } promotion={ promotion.ab } /></chess>)

    def resign(player: ServerClient) =
        op(player, game = game.resign(if (player == host) White else Black), <chess username={ player.username }><resign /></chess>, false) // ignore turn

    def drawAsk(player: ServerClient) =
        op(player, game = game.drawAsk, <chess username={ player.username }><drawAsk /></chess>)

    def drawAccept(player: ServerClient) =
        op(player, game = game.drawAccept, <chess username={ player.username }><drawaccept /></chess>)

    def drawDecline(player: ServerClient) =
        op(player, game = game.drawDecline, <chess username={ player.username }><drawdecline /></chess>)

    def checkGameConditions(player: ServerClient) = {
        if (player == host && game.turn != White || player != host && game.turn != Black) {
            throw ProtocolException("Wait your turn!");
        } else if (opponent == None) {
            throw ProtocolException("Wait for your opponent!");
        }
    }
}

case class ProtocolException(msg: String) extends RuntimeException(msg)
