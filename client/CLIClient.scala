package ChessServer.client


class CLIClient {
    import scala.collection.mutable.{HashSet,HashMap,Set}
    import logic._

    type Highlights = Map[Position, String]

    var game = new Game(20)

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
        println("Turn: "+game.turn)
        println("Status: "+game.status)
        println("Last Move: "+game.board.lastMove)
        println("White timer:"+game.times._1+" sec");
        println("Black timer:"+game.times._2+" sec");
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
                l2 = l2 + (game.board.pieceAt(Position(x,y)) match {
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
                    case MovePromote(from, to, typeTo) =>
                        game = game.moveAndPromote(from, to, typeTo);
                        draw(from :: to :: from.pathTo(to), Console.BOLD+Console.YELLOW_B)
                    case Timers =>
                        val timers = game.timers
                        println("White timer:"+timers._1+" sec");
                        println("Black timer:"+timers._2+" sec");
                    case Draw =>
                        game = game.drawAsk;
                        draw
                    case DrawAccept =>
                        game = game.drawAccept;
                        draw
                    case DrawDecline =>
                        game = game.drawDecline;
                        draw
                    case Move(from, to) =>
                        game = game.move(from, to);
                        draw(from :: to :: from.pathTo(to), Console.BOLD+Console.YELLOW_B)
                    case AnalyzeAll =>
                        draw(game.board.slots.values.filter{ _.color == game.turn }.map{ game.board.movesOptionsCheckKingSafety(_) }.reduceLeft{ (a,b) => a ++ b}, Console.WHITE_B);
                    case Analyze(pos) =>
                        game.board.pieceAt(pos) match {
                            case Some(p) => draw(game.board.movesOptionsCheckKingSafety(p), Console.WHITE_B);
                            case None => println("< Error: Can't find any piece at pos "+pos);
                        }
                        
                    case Quit => println("< Bye."); continue = false
                    case Unknown(str) => println("< \""+str+"\"?");
                }

                game.status match {
                    case _:GameEnded =>
                        println("GAME ended: "+game.status);
                        continue = false;
                    case _ =>
                }
            } catch {
                case _: java.io.EOFException =>
                    continue = false
                case e =>
                    println("< Error: "+e)
                    e.printStackTrace
            }
        }
    }

    abstract class Cmd
    case class Move(from: Position, to: Position) extends Cmd
    case class MovePromote(from: Position, to: Position, typ: PieceType) extends Cmd
    case class Analyze(pos: Position) extends Cmd
    object AnalyzeAll extends Cmd
    case class Unknown(str: String) extends Cmd
    object Quit extends Cmd
    object Draw extends Cmd
    object DrawAccept extends Cmd
    object DrawDecline extends Cmd
    object Timers extends Cmd

    def parse(str: String): Cmd = {
        try {
            str.split(" +").toList match {
                case "m"  :: pf :: pt :: Nil => Move(new Position(pf), new Position(pt))
                case "mp" :: pf :: pt :: typ :: Nil => MovePromote(new Position(pf), new Position(pt), parseType(typ))
                case "a" :: p :: Nil => Analyze(new Position(p))
                case "aa" :: Nil => AnalyzeAll
                case "q" :: Nil => Quit
                case "d" :: Nil => Draw
                case "da" :: Nil => DrawAccept
                case "dd" :: Nil => DrawDecline
                case "t" :: Nil => Timers
                case "quit" :: Nil => Quit
                case "exit" :: Nil => Quit
                case _ => Unknown(str)
            }
        } catch {
            case x => Unknown(str+"("+x.getMessage+")")
        }
    }

    def parseType(str: String): PieceType = str.toUpperCase match {
        case "Q" => Queen
        case "R" => Rook
        case "N" => Knight
        case "B" => Bishop
        case _ =>
            throw new RuntimeException("Invalid promotion type");
    }
}
