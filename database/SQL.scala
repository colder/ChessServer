package ChessServer.database

import java.sql.SQLException
import java.sql.PreparedStatement
import java.sql.ResultSet

abstract class SQLConnection {
    var conn: Option[java.sql.Connection];
    def connect;

    def prepareCheck(conn: java.sql.Connection, sql: String) = {
        try {
            conn.prepareStatement(sql)
        } catch  {
            case se: SQLException if se.getErrorCode == 2006 =>
                // Server has gone away, reconnect
                println("! Reconnecting...")
                connect
                conn.prepareStatement(sql)
            case e => throw e
        }
    }

    def prepareStatement(sql: String): SQLStatement = conn match {
        case Some(c) => new SQLStatement(this, prepareCheck(c, sql))
        case None => throw new Exception("No connection")
    }

    def prepareStatement(sql: String, args:Any*): SQLStatement = {
        checkConnection

        val stmt = conn match {
            case Some(c) => prepareCheck(c, sql)
            case None => throw new Exception("No connection")
        }

        var index = 1

        for(arg <- args) {
            arg match {
                case as: String =>
                    stmt.setString(index, as)
                    index += 1
                case ai: Int =>
                    stmt.setInt(index, ai)
                    index += 1
                case ai: Double =>
                    stmt.setDouble(index, ai)
                    index += 1
                case ai: Long =>
                    stmt.setLong(index, ai)
                    index += 1
                case aai: List[_] =>
                    for (a <- aai) {
                        a match {
                            case i: Int => 
                                stmt.setInt(index, i)
                            case s: String =>
                                stmt.setString(index, s)
                        }
                        index += 1
                    }
                case _ =>
                    throw new Exception("Invalid type of argument passed")
            }
        }

        new SQLStatement(this, stmt)
    }

    def close = conn match {
        case Some(x) => x.close; conn = None
        case None => throw new Exception("No connection");
    }

    def handleException(ex: Exception) = ex match {
        case s:SQLException =>
            println("SQLException: " + s.getMessage +" ("+s.getErrorCode+")")
            s.printStackTrace

        case e: Exception =>
            println("Exception: " + e.getMessage)
            e.printStackTrace
    }

    def checkConnection = conn match {
        case Some(c) =>
            try
            {
                c.createStatement().close();
            } catch  {
                case ex: Exception =>
                connect
            }
        case _ =>
    }
}

class SQLStatement(conn: SQLConnection, stmt: PreparedStatement) {
    def executeQuery =
        try {
            new SQLResultSet(stmt.executeQuery)
        } catch {
            case e: java.io.EOFException =>
                println("! Reconnecting...")
                conn.connect
                new SQLResultSet(stmt.executeQuery)
        }
    def executeUpdate =
        try {
            stmt.executeUpdate
        } catch {
            case e: java.io.EOFException =>
                println("! Reconnecting...")
                conn.connect
                stmt.executeUpdate
        }
    def close =
        try {
            stmt.close
        } catch {
            case e: java.io.EOFException =>
                println("! Reconnecting...")
                conn.connect
                stmt.close
        }
}

class SQLResultSet(set: ResultSet) {
    def foreach(f: ResultSet => Unit): Unit = {
        while(hasNext) f(set)
    }

    def hasNext = set.next

    def firstRow: ResultSet = { set.beforeFirst; set.next; set }
}
