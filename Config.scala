package ChessServer;

import scala.xml._

class Config(path: String) {
    private val data = XML.loadFile(path)
    private val db = data \ "db" first
    val dbUser = (db \ "@user").text
    val dbPass = (db \ "@pass").text
    val dbDatabase = (db \ "@database").text

    private val host = data \ "host" first
    val hostPort = Integer.parseInt((host \ "@port").text)
}
