package ChessServer.logic

import scala.collection.immutable.HashMap

abstract class GameStatus;
case object GameRunning  extends GameStatus;
case object GameDraw     extends GameStatus;
case object GameWinBlack extends GameStatus;
case object GameWinWhite extends GameStatus;


/* immutable */
case class Game(board: Board, turn: ChessTeam, boards: Map[Board, Int], movesWithoutCapture: Int, drawsRequests: (Boolean, Boolean), status: GameStatus) {

    def this() = this(Board.init, White, HashMap[Board, Int](), 0, (false, false), GameRunning)

    def nextTurn: Game = Game(board, if (turn == White) Black else White, boards, movesWithoutCapture, drawsRequests, status);

    def reinitBoards: Game = Game(board, turn, HashMap[Board, Int](), movesWithoutCapture, drawsRequests, status);

    def storeBoard: Game = {
        boards get board match {
            case Some(i) => Game(board, turn, boards + ((board, i+1)), movesWithoutCapture, drawsRequests, status)
            case None => Game(board, turn, boards + ((board, 1)), movesWithoutCapture, drawsRequests, status)
        }
    }

    def fromMoveResult(mr: MoveResult): Game = {
        val newBoards = if(mr.reinitBoards) HashMap[Board, Int]() else boards
        val newMovesWithoutCapture = if(mr.incMovesWC) movesWithoutCapture+1 else 1

        Game(mr.board, turn, newBoards, newMovesWithoutCapture, (false, false), status)
    }

    /* Interface to the clients */
    def move(posFrom: Position, posTo: Position): Game = {
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

    def moveAndPromote(posFrom: Position, posTo: Position, promotion: PieceType): Game = {

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
                             newGame.drawsRequests,
                             newGame.status)
                    case _ =>
                        throw GameException("woops: something went wrong!")
                }
            case _ =>
                throw GameException("Invalid promotion, piece type or position invalid")
        }
    }

    def draw: Game = {
        // If we're in a situation where a draw can be requested the game ends here
        val g = if (isDrawCondition) {
            Game(board, turn, boards, movesWithoutCapture, drawsRequests, GameDraw)
        } else {
            val drawReq = if (turn == White) (true, drawsRequests._2) else (drawsRequests._1, true);
            if (drawReq == (true, true)) {
                Game(board, turn, boards, movesWithoutCapture, drawReq, GameDraw)
            } else {
                Game(board, turn, boards, movesWithoutCapture, drawReq, GameRunning)
            }
        }

        g.nextTurn
    }

    def isDrawCondition = {
        // check for draw conditions
        val is3repetitions = boards.values exists { _ >= 3 }
        val is50moves = movesWithoutCapture >= 50

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
