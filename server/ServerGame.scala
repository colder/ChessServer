package ChessServer.server

import scala.actors.Actor
import scala.actors.Actor._
import logic.{PieceType,Position,Game}

class ServerGame(val server: Server, val host: ServerClient, val opponent: ServerClient, val ts: Long) extends Actor {
    private var game = new Game(ts)

    private var started = false

    private def otherplayer(player: ServerClient): ServerClient =
        if (player == host) { opponent } else { host }

    private def dispatch(player: ServerClient, msg: xml.Node) = otherplayer(player) ! Send(msg)

    private def end = {
        // dispatch the game ending status to both players
        val msg: Option[String => xml.Node] = game.status match {
            case logic.GameWinBlack =>
                Some(from => <chess username={ from }><end winner="black" /></chess>)
            case logic.GameWinWhite =>
                Some(from => <chess username={ from }><end winner="white" /></chess>)
            case logic.GameDraw =>
                Some(from => <chess username={ from }><draw /></chess>)
            case _ =>
                None
        }

        msg match {
            case Some(m) =>
                host ! Send(m(opponent.username))
                opponent ! Send(m(host.username))
            case None =>
        }

        server ! GameEnd(this)
    }

    private def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node): Boolean =
        op(player, action, dispatchMsg, true)

    private def op(player: ServerClient, action: => Unit, dispatchMsg: xml.Node, checkConditions: Boolean): Boolean = {
        val oppUsername = otherplayer(player).username

        if (checkConditions && (player == host && game.turn != logic.White || player != host && game.turn != logic.Black)) {
            player ! SendChessNack(oppUsername, "Wait your turn!");
            false
        } else if ( checkConditions && started == false) {
            player ! SendChessNack(oppUsername, "Wait for your opponent!");
            false
        } else {
            try {
                action
                player ! SendChessAck(oppUsername)

                // Check the status of the game
                game.status match {
                    case _ : logic.GameEnded =>
                        end
                    case _ =>
                        dispatch(player, dispatchMsg)
                }
                true
            } catch {
                case ge: logic.GameException =>
                    player ! SendChessNack(oppUsername, ge.getMessage)
                    false
            }
        }
    }

    def act() {
        var continue = true
        while (continue) {
            receive {
                case InviteAccept =>
                    started match {
                        case true =>
                            reply(Failure("This game has already started"))
                        case false =>
                            host ! Send(<chess username={ opponent.username }><inviteaccept /></chess>)
                            started = true
                            game = game.start
                            reply(Success(game))
                    }
                case InviteDecline =>
                    started match {
                        case true =>
                            reply(Failure("This game has already started"))
                        case false =>
                            host ! Send(<chess username={ opponent.username }><invitedecline /></chess>)
                            started = true
                            game = game.resign(logic.Black)
                            reply(Success(game))
                    }
                case Timers(player) =>
                    player ! Send(<chess username={ otherplayer(player).username }><timers white={ game.timers._1.toString } black={ game.timers._2.toString } /></chess>)
                case Move(player, from, to) =>
                    op(player, game = game.move(from, to), <chess username={ player.username }><move from={ from.algNotation } to={ to.algNotation } /></chess>)
                case MovePromote(player, from, to, pt) =>
                    op(player, game = game.move(from, to), <chess username={ player.username }><move from={ from.algNotation } to={ to.algNotation } promotion={ pt.ab } /></chess>)
                case Resign(player) =>
                    op(player, game = game.resign(if (player == host) logic.White else logic.Black), <chess username={ player.username }><resign /></chess>, false) // ignore turn
                case DrawAsk(player) =>
                    op(player, game = game.drawAsk, <chess username={ player.username }><drawAsk /></chess>)
                case DrawAccept(player) =>
                    op(player, game = game.drawAccept, <chess username={ player.username }><drawaccept /></chess>)
                case DrawDecline(player) =>
                    op(player, game = game.drawDecline, <chess username={ player.username }><drawdecline /></chess>)

                case CloseGame =>
                    continue = false
            }
        }

        println("ServerGame Actor terminating...")
    }

    start
}

abstract class ServerGameCommand;
case object InviteAccept extends ServerGameCommand
case object InviteDecline extends ServerGameCommand
case class  Timers(player: ServerClient) extends ServerGameCommand
case class  Move(player: ServerClient, from: Position, to: Position) extends ServerGameCommand
case class  MovePromote(player: ServerClient, from: Position, to: Position, pt: PieceType) extends ServerGameCommand
case class  Resign(player: ServerClient) extends ServerGameCommand
case class  DrawAsk(player: ServerClient) extends ServerGameCommand
case class  DrawAccept(player: ServerClient) extends ServerGameCommand
case class  DrawDecline(player: ServerClient) extends ServerGameCommand
case object CloseGame extends ServerGameCommand


case class ProtocolException(msg: String) extends RuntimeException(msg)
