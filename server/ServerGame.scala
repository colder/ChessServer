package ChessServer.server

class ServerGame(val server: Server, val host: ServerClient, val ts: Long) {
    import logic._

    var game = new Game(ts)
    var opponent: Option[ServerClient] = None

    def started = opponent != None

    def join(player: ServerClient): Result[_ <: Game] = opponent match {
        case Some(op) =>
            Failure("This game is already full")
        case None =>
            opponent = Some(player)
            host.onJoin(this, player)
            game = game.start
            dispatch(player, <chess username={ player.username }><join timers={ ts.toString } /></chess>)
            Success(game)
    }

    def otherplayer(player: ServerClient): Option[ServerClient] = {
        if (player == host) {
            opponent match {
                case Some(otherplayer) => Some(otherplayer)
                case None => None
            }
        } else {
            Some(host)
        }
    }

    def dispatch(player: ServerClient, msg: xml.Node) = otherplayer(player) match {
        case Some(pl) => pl.send(msg.toString)
        case None => println("! Trying to dispatch to innexistent opponent")
    }

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
        val oppUsername = otherplayer(player) map {_.username} getOrElse ""

        if (checkConditions && (player == host && game.turn != White || player != host && game.turn != Black)) {
            player.sendChessNack(oppUsername ,"Wait your turn!");
            false
        } else if ( checkConditions && opponent == None) {
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

    def timers(player: ServerClient) = {
        player.send(<chess username={ otherplayer(player) map ( _.username ) getOrElse "" }><timers white={ game.timers._1.toString } black={ game.timers._2.toString } /></chess>)
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
