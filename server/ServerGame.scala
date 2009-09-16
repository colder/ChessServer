package ChessServer.server

class ServerGame(val server: Server, val host: ServerClient, val opponent: ServerClient, val ts: Long) {
    import logic._

    private var game = new Game(ts)

    private var started = false

    private def otherplayer(player: ServerClient): ServerClient =
        if (player == host) { opponent } else { host }

    private def dispatch(player: ServerClient, msg: xml.Node) = otherplayer(player).send(msg.toString)

    private def end = {
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
            case Some(m) =>
                host.send(m(opponent.username))
                opponent.send(m(host.username))
            case None =>
        }

        server.gameEnd(this);
    }

    private def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node): Boolean =
        op(player, action, dispatchMsg, true)

    private def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node, checkConditions: Boolean): Boolean = {
        val oppUsername = otherplayer(player).username

        if (checkConditions && (player == host && game.turn != White || player != host && game.turn != Black)) {
            player.sendChessNack(oppUsername ,"Wait your turn!");
            false
        } else if ( checkConditions && started == false) {
            player.sendChessNack(oppUsername,"Wait for your opponent!");
            false
        } else {
            try {
                action
                player.send(<chess username={ oppUsername }><ack /></chess>)

                // Check the status of the game
                game.status match {
                    case _ : GameEnded =>
                        end
                    case _ =>
                        dispatch(player, dispatchMsg)
                }
                true
            } catch {
                case ge: GameException =>
                    player.sendChessNack(oppUsername, ge.getMessage)
                    false
            }
        }
    }

    def inviteaccept: Result[_ <: Game] = started match {
        case true =>
            Failure("This game has already started")
        case false =>
            host.send(<chess username={ opponent.username }><inviteaccept /></chess>)
            started = true
            game = game.start
            Success(game)
    }

    def invitedecline: Result[_ <: Game] = started match {
        case true =>
            Failure("This game has already started")
        case false =>
            host.send(<chess username={ opponent.username }><invitedecline /></chess>)
            started = true
            game = game.resign(Black)
            Success(game)
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
}

case class ProtocolException(msg: String) extends RuntimeException(msg)
