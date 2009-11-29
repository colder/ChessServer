package ChessServer.logic

import scala.collection.immutable.HashMap

abstract class GameStatus;
abstract class GameRunning extends GameStatus;
abstract class GameEnded extends GameStatus;
case object GameInit extends GameStatus;

case object GamePlaying     extends GameRunning;
case object GameDrawRequest extends GameRunning;

case object GameCancelled extends GameEnded;
case object GameDraw      extends GameEnded;
case object GameWinBlack  extends GameEnded;
case object GameWinWhite  extends GameEnded;


/* immutable */
case class Game(
    board: Board,
    turn: ChessTeam,
    boards: Map[String, Int],
    movesWithoutCapture: Int,
    status: GameStatus,
    times: (Long, Long),
    turnStartTime: Long) {

    def this(d: Long) = this(Board.init, White, new HashMap[String, Int](), 0, GameInit, (d*60, d*60), System.currentTimeMillis/1000)

    private def now = System.currentTimeMillis/1000;

    private def nextTurn: Game = {
        var nextTurn: ChessTeam = White;

        if (turn == White) {
            nextTurn = Black
        }

        Game(board, nextTurn, boards, movesWithoutCapture, status, times, turnStartTime).updateTimers;
    }

    private def setBoards(bs: Map[String, Int]): Game =
        Game(board, turn, bs, movesWithoutCapture, status, times, turnStartTime)

    private def setStatus(st: GameStatus): Game =
        Game(board, turn, boards, movesWithoutCapture, st, times, turnStartTime)

    private def reinitBoards: Game = setBoards(HashMap[String, Int]());

    private def storeBoard: Game = {
        boards get board.serialize match {
            case Some(i) => /*println("Updating "+board.serialize+" to "+(i+1));*/ setBoards(boards + ((board.serialize, i+1)))
            case None => setBoards(boards + ((board.serialize, 1)))
        }
    }

    private def fromMoveResult(mr: MoveResult): Game = {
        val newBoards = if(mr.reinitBoards) HashMap[String, Int]() else boards
        val newMovesWithoutCapture = if(mr.incMovesWC) movesWithoutCapture+1 else 1

        Game(mr.board, turn, newBoards, newMovesWithoutCapture, status, times, turnStartTime).storeBoard
    }

    /* Interface to the clients */

    def updateTimers: Game = {
        var nextTimes  = newTimers;
        var nextStatus = status;

        if (nextTimes._1 < 0) {
            nextStatus = GameWinBlack
        } else if (nextTimes._2 < 0 ) {
            nextStatus = GameWinWhite
        }

        Game(board, turn, boards, movesWithoutCapture, nextStatus, nextTimes, now);
    }

    def newTimers: (Long, Long) = status match {
        case s: GameRunning => 
            if (turn == White) {
                (times._1-(now-turnStartTime), times._2)
            } else {
                (times._1, times._2-(now-turnStartTime))
            }
        case _ =>
            times
    }

    def start: Game = if (status == GameInit) {
        Game(board, turn, boards, movesWithoutCapture, GamePlaying, times, now).storeBoard
    } else {
        throw GameException("Cannot start a game again: "+status);
    }
    def move(posFrom: Position, posTo: Position): Game = {
        if (status == GamePlaying) {
            // Check that the turn is valid
            val g = board pieceAt posFrom match {
                case Some(Piece(color, _, _, _)) if color != turn =>
                    throw GameException("Wait your turn!");
                case Some(Piece(White, Pawn, Position(_, 7), _)) =>
                    throw GameException("You can't move without promoting your pawn!");
                case Some(Piece(Black, Pawn, Position(_, 2), _)) =>
                    throw GameException("You can't move without promoting your pawn!");
                case _ =>
                    val moveResult = board.performMove(posFrom, posTo);
                    fromMoveResult(moveResult).nextTurn
            }

            // Check for checkmate
            if (g.board.isCheckMate(g.turn)) {
                g.setStatus(if (g.turn == White) GameWinBlack else GameWinWhite)
            } else {
                g
            }
        } else {
            throw GameException("Can't move in this status: "+status);
        }
    }

    def moveAndPromote(posFrom: Position, posTo: Position, promotion: PieceType): Game = {
        if (status == GamePlaying) {
            val g = board pieceAt posFrom match {
                case Some(Piece(color, _, _, _)) if color != turn =>
                    throw GameException("Wait your turn!");
                case Some(Piece(White, Pawn, Position(_, 7), _)) | Some(Piece(Black, Pawn, Position(_, 2), _)) =>
                    val moveResult = board.performMove(posFrom, posTo);
                    val newGame = fromMoveResult(moveResult).nextTurn
                    newGame.board pieceAt posTo match {
                        case Some(pi) =>
                            Game(newGame.board.promote(pi, promotion),
                                 newGame.turn,
                                 HashMap[String, Int](),
                                 newGame.movesWithoutCapture,
                                 newGame.status,
                                 newGame.times,
                                 newGame.turnStartTime)
                        case _ =>
                            throw GameException("woops: something went wrong!")
                    }
                case _ =>
                    throw GameException("Invalid promotion, piece type or position invalid")
            }

            // Check for checkmate
            if (g.board.isCheckMate(g.turn)) {
                g.setStatus(if (g.turn == White) GameWinBlack else GameWinWhite)
            } else {
                g
            }
        } else {
            throw GameException("Can't move in this state: "+status);
        }
    }

    def resign(loser: ChessTeam): Game = status match {
        case _: GameRunning =>
            if (loser == White) {
                setStatus(GameWinBlack).nextTurn
            } else {
                setStatus(GameWinWhite).nextTurn
            }
        case GameInit =>
            setStatus(GameCancelled).nextTurn
        case _ =>
            throw GameException("Can't resign when not playing!");
    }

    def drawAccept: Game = if (status == GameDrawRequest) {
        setStatus(GameDraw).nextTurn
    } else {
        throw GameException("Can't accept a draw without a draw request!");
    }

    def drawDecline: Game = if (status == GameDrawRequest) {
        setStatus(GamePlaying).nextTurn
    } else {
        throw GameException("Can't decline a draw without a draw request!");
    }

    def drawAsk: Game = {
        if (status == GamePlaying) {
            if (isDrawCondition) {
                setStatus(GameDraw).nextTurn
            } else {
                setStatus(GameDrawRequest).nextTurn
            }
        } else {
            throw GameException("Can't ask/request a draw in this state: "+status);
        }
    }

    def is3repetitions = boards.values exists { _ >= 3 }

    def is50moves = movesWithoutCapture >= 99

    def isDrawCondition = {
        is50moves || is3repetitions || insufficientForCheckmate
    }

    def insufficientForCheckmate = {
        // King vs King
        val kk = board.slots.size == 2

        // King vs King + Bishop
        val kkb = board.slots.size == 3 && board.slots.values.exists(_.typ == Bishop)

        // King vs King + Knight
        val kkn = board.slots.size == 3 && board.slots.values.exists(_.typ == Bishop)

        // King + Bishop(s) vs King + Bishop(s) where the bishops are on the same color
        val kkbb = if(board.slots.values.forall(p => p.typ == Bishop || p.typ == King)) {
            def squareColor(pi: Piece) = if ((pi.pos.x+pi.pos.y)%2 == 0) Black else White;

            val bs = board.slots.values.filter(_.typ == Bishop)
            val sqbs = squareColor(bs.next)

            bs.forall(p => squareColor(p) == sqbs)
        } else {
            false
        }

        kk || kkb ||kkn || kkbb
    }
}

case class GameException(msg: String) extends RuntimeException(msg)
