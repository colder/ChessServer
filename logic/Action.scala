package ChessServer.logic

abstract class Action
case object Normal extends Action
case class EnPassant(pos: Position) extends Action
case class Castling(pos: Position) extends Action
case object Promote extends Action

