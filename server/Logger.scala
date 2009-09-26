package ChessServer.server

import java.text.SimpleDateFormat
import java.util.Date

abstract class Logger {
    def out(msg: String);
    def in(msg: String);
    def info(msg: String);
    def err(msg: String);
    def warn(msg: String);
}

class TerminalLogger extends Logger {
    val df = new SimpleDateFormat("dd-MMM-yy HH:mm:ss")
    def date = df.format(new Date())

    def out(msg: String)  = println(date+" [>] "+msg)
    def info(msg: String)   = println(date+" [-] "+msg)
    def in(msg: String)   = println(date+" [<] "+msg)
    def err(msg: String)  = println(date+" [!] "+msg)
    def warn(msg: String) = println(date+" [w] "+msg)
}
