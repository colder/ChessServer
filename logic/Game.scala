package ChessServer.logic

import scala.collection.immutable.HashMap

abstract class GameStatus;
abstract class GameRunning extends GameStatus;
abstract class GameEnded extends GameStatus;

case object GamePlaying     extends GameRunning;
case object GameDrawRequest extends GameRunning;

case object GameDraw     extends GameEnded;
case object GameWinBlack extends GameEnded;
case object GameWinWhite extends GameEnded;


/* immutable */
case class Game(
    board: Board,
    turn: ChessTeam,
    boards: Map[Board, Int],
    movesWithoutCapture: Int,
    status: GameStatus,
    times: (Long, Long),
    turnStartTime: Long) extends ServerInterface {

    def this(d: Long) = this(Board.init, White, HashMap[Board, Int](), 0, GamePlaying, (d*60, d*60), System.currentTimeMillis/1000)

    private def now = System.currentTimeMillis/1000;

    private def nextTurn: Game = {
        var nextTurn: ChessTeam = White;
        var nextTimes  = timers;
        var nextStatus = status;

        if (turn == White) {
            nextTurn = Black
        }

        if (nextTimes._1 < 0) {
            nextStatus = GameWinBlack
        } else if (nextTimes._2 < 0 ) {
            nextStatus = GameWinWhite
        }
        Game(board, nextTurn, boards, movesWithoutCapture, nextStatus, nextTimes, now);
    }

    private def setBoards(bs: Map[Board, Int]): Game =
        Game(board, turn, bs, movesWithoutCapture, status, times, turnStartTime)

    private def setStatus(st: GameStatus): Game =
        Game(board, turn, boards, movesWithoutCapture, st, times, turnStartTime)

    private def reinitBoards: Game = setBoards(HashMap[Board, Int]());

    private def storeBoard: Game = {
        boards get board match {
            case Some(i) => setBoards(boards + ((board, i+1)))
            case None => setBoards(boards + ((board, 1)))
        }
    }

    private def fromMoveResult(mr: MoveResult): Game = {
        val newBoards = if(mr.reinitBoards) HashMap[Board, Int]() else boards
        val newMovesWithoutCapture = if(mr.incMovesWC) movesWithoutCapture+1 else 1

        Game(mr.board, turn, newBoards, newMovesWithoutCapture, status, times, turnStartTime)
    }

    /* Interface to the clients */
    def timers: (Long, Long) = if (turn == White) {
        (times._1-(now-turnStartTime), times._2)
    } else {
        (times._1, times._2-(now-turnStartTime))
    }

    def move(posFrom: Position, posTo: Position): Game = {
        if (status == GamePlaying) {
            val ts = timers
            if (ts._1 <= 0) {
                setStatus(GameWinBlack)
            } else if (ts._2 <= 0) {
                setStatus(GameWinWhite)
            } else {
                // Check that the turn is valid
                board pieceAt posFrom match {
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
            }
        } else {
            throw GameException("Can't move in this status: "+status);
        }
    }

    def moveAndPromote(posFrom: Position, posTo: Position, promotion: PieceType): Game = {
        if (status == GamePlaying) {
            val ts = timers
            if (ts._1 <= 0) {
                setStatus(GameWinBlack)
            } else if (ts._2 <= 0) {
                setStatus(GameWinWhite)
            } else {
                board pieceAt posFrom match {
                    case Some(Piece(color, _, _, _)) if color != turn =>
                        throw GameException("Wait your turn!");
                    case Some(Piece(White, Pawn, Position(_, 7), _)) | Some(Piece(Black, Pawn, Position(_, 2), _)) =>
                        val moveResult = board.performMove(posFrom, posTo);
                        val newGame = fromMoveResult(moveResult).nextTurn
                        newGame.board pieceAt posTo match {
                            case Some(pi) =>
                                Game(newGame.board.promote(pi, promotion),
                                     newGame.turn,
                                     HashMap[Board, Int](),
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
            }
        } else {
            throw GameException("Can't move in this state: "+status);
        }
    }

    def drawAccept: Game = if (status == GameDrawRequest) {
        setStatus(GameDraw).nextTurn
    } else {
        throw GameException("Can't a draw without a draw request!");
    }

    def drawDecline: Game = if (status == GameDrawRequest) {
        setStatus(GamePlaying).nextTurn
    } else {
        throw GameException("Can't a draw without a draw request!");
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

    private def isDrawCondition = {
        // check for draw conditions
        val is3repetitions = boards.values exists { _ >= 3 }
        val is50moves = movesWithoutCapture >= 50

        is50moves || is3repetitions || insufficientForCheckmate
    }

    private def insufficientForCheckmate = {
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
