package ChessServer.client

import java.net._
import xml._
import java.io.{BufferedReader,InputStreamReader,PrintWriter}

class CLIClient {
    import scala.collection.mutable.{HashSet,HashMap,Set}
    import logic._

    type Highlights = Map[Position, String]

    var game = new Game(20)
    var loginSeed = ""

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

        val sock = new Socket("localhost", 12345);
        val out = new PrintWriter(sock.getOutputStream(), true);
        val in = new BufferedReader(new InputStreamReader(sock.getInputStream()));

        def isNack: Option[String] = {
            val reply = XML.loadString(in.readLine)
            reply match {
                case <ack /> =>
                    None
                case Elem(_, "nack", attr, _) =>
                    if (attr("msg") != null) {
                        Some(attr("msg").toString)
                    } else {
                        Some("")
                    }
            }
        }
        XML.loadString(in.readLine) match {
            case Elem(_, "hello", attr, _) =>
                println("Hello, Seed is "+attr("seed"))
                loginSeed = attr("seed").toString
            case x =>
                println(x)
        }

        draw
        while(continue) {
            try {
                print("> ")
                val cmd = parse(Console.readLine)

                cmd match {
                    case GamesList =>
                        out.println(<games><list /></games>);
                        val reply = XML.loadString(in.readLine)
                        reply match {
                            case Elem(_, "nack", attr, _) =>
                                println("Error: "+attr("msg"))
                            case _ =>
                                println(reply)
                        }
                    case GamesCreate(timers) =>
                        out.println(<games><create timers="{ timers }" /></games>);
                        isNack match {
                            case None =>
                                game = new Game(timers)
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case Login(user, pass) =>
                        val passwordHashed = server.Hash.sha1(pass+loginSeed);
                        println("Hasing "+pass+" with "+loginSeed+" => "+passwordHashed);
                        out.println(<auth><login username={ user } challenge={ passwordHashed } /></auth>);
                        isNack match {
                            case None =>
                                println("Logged On!");
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case GamesJoin(host) =>
                        out.println(<games><join host="{ host }" /></games>);
                        isNack match {
                            case None =>
                                game = new Game(20)
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case MovePromote(from, to, typeTo) =>
                        out.println(<game><move from="{ from.algNotation }" to="{ to.algNotation }" promotion="{ typeTo.ab }" /></game>);
                        isNack match {
                            case None =>
                                game.moveAndPromote(from, to, typeTo)
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case Timers =>
                        out.println(<game><timers /></game>);
                        val reply = XML.loadString(in.readLine)
                        println(reply)
                    case Draw =>
                        out.println(<game><drawask /></game>);
                        isNack match {
                            case None =>
                                game.drawAsk
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case DrawAccept =>
                        out.println(<game><drawaccept /></game>);
                        isNack match {
                            case None =>
                                game.drawAccept
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case DrawDecline =>
                        out.println(<game><drawdecline /></game>);
                        isNack match {
                            case None =>
                                game.drawDecline
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case Move(from, to) =>
                        out.println(<game><move from={ from.algNotation } to={ to.algNotation } /></game>);
                        isNack match {
                            case None =>
                                game.move(from, to)
                            case Some(x) =>
                                println("Error: "+x);
                        }
                    case AnalyzeAll =>
                        draw(game.board.slots.values.filter{ _.color == game.turn }.map{ game.board.movesOptionsCheckKingSafety(_) }.reduceLeft{ (a,b) => a ++ b}, Console.WHITE_B);
                    case Analyze(pos) =>
                        game.board.pieceAt(pos) match {
                            case Some(p) => draw(game.board.movesOptionsCheckKingSafety(p), Console.WHITE_B);
                            case None => println("< Error: Can't find any piece at pos "+pos);
                        }
                    case Quit => 
                        println("< Bye.")
                        out.println(<quit />);
                        continue = false
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
    object GamesList extends Cmd
    case class GamesJoin(host: String) extends Cmd
    case class GamesCreate(timers: Int) extends Cmd
    case class Login(username: String, password: String) extends Cmd

    def parse(str: String): Cmd = {
        try {
            str.split(" +").toList match {
                case "m"  :: pf :: pt :: Nil => Move(new Position(pf), new Position(pt))
                case "mp" :: pf :: pt :: typ :: Nil => MovePromote(new Position(pf), new Position(pt), parseType(typ))
                case "a" :: p :: Nil => Analyze(new Position(p))
                case "aa" :: Nil => AnalyzeAll
                case "gl" :: Nil => GamesList
                case "gc" :: t ::  Nil => GamesCreate(t.toInt)
                case "gj" :: host ::  Nil => GamesJoin(host)
                case "q" :: Nil => Quit
                case "d" :: Nil => Draw
                case "da" :: Nil => DrawAccept
                case "dd" :: Nil => DrawDecline
                case "t" :: Nil => Timers
                case "l" :: username :: password :: Nil => Login(username, password)
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
