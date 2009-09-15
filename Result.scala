package ChessServer

abstract class Result[T];

case class Success[T](value: T) extends Result[T];
case class Failure(error: String) extends Result[Nothing];
