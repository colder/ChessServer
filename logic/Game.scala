package ChessServer.logic

import scala.collection.immutable.HashMap

/* immutable */
case class Game(board: Board, turn: ChessTeam, boards: Map[Board, Int], movesWithoutCapture: Int) {

    def this() = this(Board.init, White, HashMap[Board, Int](), 0)

    def nextTurn: Game = Game(board, if (turn == White) Black else White, boards, movesWithoutCapture);

    def reinitBoards: Game = Game(board, turn, new HashMap[Board, Int](), movesWithoutCapture);

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
        board pieceAt(posFrom) match {
            case Some(p) if p.color != turn =>
                throw GameException("Wait your turn!");
            case _ =>
                val moveResult = board.performMove(posFrom, posTo);
                fromMoveResult(moveResult).nextTurn
        }

    }

    def moveAndPromote(from: Position, to: Position, promotion: PieceType): Game = {
        val newGame = move(from, to)

        newGame.board.pieceAt(to) match {
            case Some(pi) =>
                Game(newGame.board.promote(pi, promotion), turn, boards, movesWithoutCapture).reinitBoards
            case None =>
                throw GameException("Wooups: no piece available for promotion, strange!")
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
