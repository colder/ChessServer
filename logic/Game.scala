package ChessServer.logic

import scala.collection.immutable.HashMap

/* immutable */
case class Game(board: Board, turn: ChessTeam, boards: Map[Board, Int], movesWithoutCapture: Int) {

    def this() = this(Board.init, White, HashMap[Board, Int](), 0)

    def nextTurn: Game = Game(board, if (turn == White) Black else White, boards, movesWithoutCapture);

    def reinitBoards: Game = Game(board, turn, HashMap[Board, Int](), movesWithoutCapture);

    def storeBoard: Game = {
        boards get board match {
            case Some(i) => Game(board, turn, boards + ((board, i+1)), movesWithoutCapture)
            case None => Game(board, turn, boards + ((board, 1)), movesWithoutCapture)
        }
    }

    def fromMoveResult(mr: MoveResult): Game = {
        val newBoards = if(mr.reinitBoards) HashMap[Board, Int]() else boards
        val newMovesWithoutCapture = if(mr.incMovesWC) movesWithoutCapture+1 else 1

        Game(mr.board, turn, newBoards, newMovesWithoutCapture)
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
                        Game(newGame.board.promote(pi, promotion), newGame.turn, HashMap[Board, Int](), newGame.movesWithoutCapture)
                    case _ =>
                        throw GameException("woops: something went wrong!")
                }
            case _ =>
                throw GameException("Invalid promotion, piece type or position invalid")
        }
    }

    def isDrawCondition = {
        // check for draw conditions
        val is3repetitions = boards.values exists { _ >= 3 }
        val is50moves = movesWithoutCapture >= 50

        is50moves || is3repetitions
    }
}

case class GameException(msg: String) extends RuntimeException(msg)
