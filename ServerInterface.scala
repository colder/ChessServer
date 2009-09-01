package ChessServer

import logic.{Game,PieceType,Position}

trait ServerInterface {
    def move(posFrom: Position, posTo: Position): Game;

    def moveAndPromote(posFrom: Position, posTo: Position, promotion: PieceType): Game;

    def drawAccept: Game;

    def drawDecline: Game;

    def drawAsk: Game;
}
