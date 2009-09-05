package ChessServer.server
import java.security.MessageDigest

object Hash {
    def sha1(in: String): String = {
        val md = MessageDigest.getInstance("SHA")
        md.update(in.getBytes)
        val bytesRaw = md.digest
        var result = "";
        for (b <- bytesRaw) {
            val hex = (b & 0xff).toInt.toHexString
            result += (if (hex.length == 1) "0"+hex else hex)
        }

        result
    }
}
