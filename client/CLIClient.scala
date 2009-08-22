package ChessServer.client

import scala.collection.mutable.{HashSet,HashMap,Set}

class CLIClient {
    import logic._

    type Highlights = Map[Position, String]

    var board = Board.init

    def draw: Unit = draw(Map[Position, String]());

    def draw(highlights: Iterable[Position]): Unit = draw(highlights, Console.YELLOW_B);

    def draw(highlights: Iterable[Position], color: String): Unit = {
        //val m = new Highlights()
        //m ++= (highlights map {(_, color)})
        draw(Map[Position, String]()++(highlights map {(_, color)}));
    }

    def draw(highlights: Highlights): Unit = {
        def line = println("     +-----+-----+-----+-----+-----+-----+-----+------")

        println
        println("Turn: "+board.turn)
        println("Last Move: "+board.lastMove)
        println
        println
        println("     "+((0 to 7) map { x: Int => "   "+('A'+x).toChar+"  " }).mkString)
        println
        line
        for (yb <- 1 to 8) {
            val y = 9-yb

            var l1 = "     ";
            var l2 = "  "+y+"  ";
            var l3 = "     ";

            for (x <- 1 to 8) {
                val highlight = highlights get Position(x,y) match { case Some(s) => s case None => "" };

                l1 = l1 + "|"+highlight+"     "+Console.RESET
                l2 = l2 + (board.slots.get(Position(x,y)) match {
                    case Some(p) =>
                        val color = if (p.color == White) Console.GREEN else Console.RED;
                    "|"+highlight+"  "+color+p.typ.ab+"  "+Console.RESET;
                    case None => "|"+highlight+"     "+Console.RESET;
                })
                l3 = l3 + "|"+highlight+"     "+Console.RESET
            }
            println(l1+"|");
            println(l2+"|");
            println(l3+"|");
            line
        }

        println
        println("     "+((0 to 7) map { x: Int => "   "+('A'+x).toChar+"  " }).mkString)
        println

    }

    def start() = {
        var continue = true;
        draw
        while(continue) {
            try {
                print("> ")
                val cmd = parse(Console.readLine)

                cmd match {
                    case Move(from, to) =>
                        board = board.movePiece(from,to)
                        draw(from :: to :: board.path(from, to), Console.BOLD+Console.YELLOW_B)
                    case Analyze(pos) =>
                        board.slots get pos match {
                            case Some(p) => draw(board.movesOptionsCheckKingSafety(p) map {_._1}, Console.WHITE_B);
                            case None => println("< Error: Can't find any piece at pos "+pos);
                        }
                        
                    case Quit => println("< Bye."); continue = false
                    case Unknown(str) => println("< \""+str+"\"?");
                }
            } catch {
                case _: java.io.EOFException =>
                    continue = false
                case e =>
                    println("< Error: "+e.getMessage)
            }
        }
    }

    abstract class Cmd
    case class Move(from: Position, to: Position) extends Cmd
    case class Analyze(pos: Position) extends Cmd
    case class Unknown(str: String) extends Cmd
    object Quit extends Cmd

    def parse(str: String): Cmd = {
        try {
            str.split(" +").toList match {
                case "m" :: pf :: pt :: Nil => Move(new Position(pf), new Position(pt))
                case "a" :: p :: Nil => Analyze(new Position(p))
                case "q" :: Nil => Quit
                case "quit" :: Nil => Quit
                case "exit" :: Nil => Quit
                case _ => Unknown(str)
            }
        } catch {
            case x => Unknown(str+"("+x.getMessage+")")
        }
    }
}
