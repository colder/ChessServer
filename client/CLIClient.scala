package ChessServer.client

import java.net._
import xml._
import java.io.{BufferedReader,InputStreamReader,PrintWriter}

class CLIClient {
    import scala.collection.mutable.{HashSet,HashMap,Set}
    import logic._

    type Highlights = Map[Position, String]

    var game = new Game(20)
    var playingTeam: ChessTeam = White

    var loginSalt = ""

    def display: Unit = display(Map[Position, String]());

    def display(highlights: Iterable[Position]): Unit = display(highlights, Console.YELLOW_B);

    def display(highlights: Iterable[Position], color: String): Unit = {
        //val m = new Highlights()
        //m ++= (highlights map {(_, color)})
        display(Map[Position, String]()++(highlights map {(_, color)}));
    }

    def display(highlights: Highlights): Unit = {
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

        def executeCommand(cmd: Cmd) = {
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
                    out.println(<games><create timers={ timers.toString } /></games>);
                    isNack match {
                        case None =>
                            game = new Game(timers)
                            playingTeam = White
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case Login(user, pass) =>
                    val passwordHashed = server.Hash.sha1(pass+loginSalt);
                    out.println(<auth><login username={ user } challenge={ passwordHashed } /></auth>);
                    isNack match {
                        case None =>
                            println("Logged On!");
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case GamesJoin(username) =>
                    out.println(<games><join username={ username } /></games>);
                    isNack match {
                        case None =>
                            game = new Game(20).start
                            playingTeam = Black
                            display
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case MovePromote(from, to, typeTo) =>
                    out.println(<game><move from={ from.algNotation } to={ to.algNotation } promotion={ typeTo.ab } /></game>);
                    isNack match {
                        case None =>
                            game = game.moveAndPromote(from, to, typeTo)
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case Timers =>
                    out.println(<game><timers /></game>);
                    val reply = XML.loadString(in.readLine)
                    println(reply)
                case Resign =>
                    out.println(<game><resign /></game>);
                    isNack match {
                        case None =>
                            game = game.resign(playingTeam)
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case Draw =>
                    out.println(<game><drawask /></game>);
                    isNack match {
                        case None =>
                            game = game.drawAsk
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case DrawAccept =>
                    out.println(<game><drawaccept /></game>);
                    isNack match {
                        case None =>
                            game = game.drawAccept
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case DrawDecline =>
                    out.println(<game><drawdecline /></game>);
                    isNack match {
                        case None =>
                            game = game.drawDecline
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case Logout =>
                    out.println(<auth><logout /></auth>);
                    isNack match {
                        case None =>
                            println("logged out!");
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case Msg(to, content) =>
                    out.println(<chat><msg username={ to } >{ content }</msg></chat>);
                    isNack match {
                        case None =>
                        case Some(x) =>
                            println("Error: "+x);
                    }

                case Move(from, to) =>
                    out.println(<game><move from={ from.algNotation } to={ to.algNotation } /></game>);
                    isNack match {
                        case None =>
                            game = game.move(from, to)
                        case Some(x) =>
                            println("Error: "+x);
                    }
                case Display =>
                    display
                case Noop =>
                case AnalyzeAll =>
                    display(game.board.slots.values.filter{ _.color == game.turn }.map{ game.board.movesOptionsCheckKingSafety(_) }.reduceLeft{ (a,b) => a ++ b}, Console.WHITE_B);
                case Analyze(pos) =>
                    game.board.pieceAt(pos) match {
                        case Some(p) => display(game.board.movesOptionsCheckKingSafety(p), Console.WHITE_B);
                        case None => println("< Error: Can't find any piece at pos "+pos);
                    }
                case Quit => 
                    println("< Bye.")
                    out.println(<quit />);
                    continue = false
                case Unknown(str) => println("< \""+str+"\"?");
            }
        }

        XML.loadString(in.readLine) match {
            case Elem(_, "hello", attr, _) =>
                loginSalt = attr("salt").toString
            case x =>
                println(x)
        }


        while(continue) {
            try {
                // Is there anything on the line?
                while (in.ready) {
                    val cmd = in.readLine
                    println("(<) "+cmd);
                    XML.loadString(cmd) match {
                        case <game>{ c }</game> =>
                            c match {
                                case Elem(_, "move", attr, _) =>
                                    if (attr.get("from") != None && attr.get("to") != None) {
                                        game = game.move(
                                                    new Position(attr("from").toString),
                                                    new Position(attr("to").toString))
                                    } else {
                                        println("woops!")
                                    }
                                case Elem(_, "movepromote", attr, _) =>
                                    def parsePromotion(str: String): PieceType = str.toUpperCase match {
                                        case "Q" => Queen
                                        case "R" => Rook
                                        case "N" => Knight
                                        case "B" => Bishop
                                        case _ =>
                                            throw new RuntimeException("Invalid promotion type");
                                    }
                                    if (attr.get("from") != None && attr.get("to") != None && attr.get("promotion") != None) {
                                        try {
                                            game = game.moveAndPromote(
                                                                    new Position(attr("from").toString),
                                                                    new Position(attr("to").toString),
                                                                    parsePromotion(attr("promotion").toString))
                                        } catch {
                                            case e: RuntimeException=>
                                                println("woops!")
                                        }
                                    } else {
                                        println("Invalid game.movepromote command");
                                    }
                                case Elem(_, "resign", _, _) =>
                                        game = game.resign(if (playingTeam == White) Black else White)
                                case Elem(_, "drawask", _, _) =>
                                        game = game.drawAsk
                                case Elem(_, "drawaccept", _, _) =>
                                        game = game.drawAccept
                                case Elem(_, "drawdecline", _, _) =>
                                        game = game.drawDecline
                                case Elem(_, "joined", _, _) =>
                                        game = game.start
                                        display
                                case x =>
                                    println("ignoring: "+x)
                            }

                            case x =>
                                println("ignoring: "+x)
                    }
                }
                print("> ")
                val cmd = parse(Console.readLine)

                executeCommand(cmd)

                game.status match {
                    case _:GameEnded =>
                        println("GAME ended: "+game.status);
                        game = new Game(20);
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
    object Display extends Cmd
    object Draw extends Cmd
    object DrawAccept extends Cmd
    object DrawDecline extends Cmd
    object Timers extends Cmd
    object Resign extends Cmd
    object Logout extends Cmd
    object GamesList extends Cmd
    object Noop extends Cmd
    case class Msg(to: String, content:String) extends Cmd
    case class GamesJoin(host: String) extends Cmd
    case class GamesCreate(timers: Int) extends Cmd
    case class Login(username: String, password: String) extends Cmd

    def parse(str: String): Cmd = {
        try {
            str.split(" ").toList match {
                case "m"  :: pf :: pt :: Nil => Move(new Position(pf), new Position(pt))
                case "mp" :: pf :: pt :: typ :: Nil => MovePromote(new Position(pf), new Position(pt), parseType(typ))
                case "msg" :: to :: msg => Msg(to, msg.mkString(" "))
                case "a" :: p :: Nil => Analyze(new Position(p))
                case "aa" :: Nil => AnalyzeAll
                case "gl" :: Nil => GamesList
                case "gc" :: t ::  Nil => GamesCreate(t.toInt)
                case "gj" :: host ::  Nil => GamesJoin(host)
                case "q" :: Nil => Quit
                case "di" :: Nil => Display
                case "d" :: Nil => Draw
                case "da" :: Nil => DrawAccept
                case "dd" :: Nil => DrawDecline
                case "t" :: Nil => Timers
                case "login" :: username :: password :: Nil => Login(username, password)
                case "resign" :: Nil => Resign
                case "logout" :: Nil => Logout
                case "quit" :: Nil => Quit
                case "exit" :: Nil => Quit
                case "nop" :: Nil => Noop
                case "noop" :: Nil => Noop
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
