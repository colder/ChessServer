package ChessServer.server
import java.security.MessageDigest

object Hash {
    def sha1(in: String): String = {
        val md = MessageDigest.getInstance("SHA")
        md.update(in.getBytes)
        val bytesRaw = md.digest
        val chars = encode(bytesRaw, bytesRaw.length)

        chars.mkString
    }

    val map1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toList;

    def encode(in: Array[Byte], iLen: Int): Array[Char] = {
       val oDataLen = (iLen*4+2)/3;       // output length without padding
       val oLen = ((iLen+2)/3)*4;         // output length including padding
       var out: Array[Char] = new Array[Char](oLen);
       var ip = 0;
       var op = 0;
       while (ip < iLen) {
          val i0 = in(ip) & 0xff
          ip+=1
          val i1 = if (ip < iLen) { val ipb = ip; ip+=1; in(ipb) & 0xff } else 0;
          val i2 = if (ip < iLen) { val ipb = ip; ip+=1; in(ipb) & 0xff } else 0;
          val o0 = i0 >>> 2;
          val o1 = ((i0 &   3) << 4) | (i1 >>> 4);
          val o2 = ((i1 & 0xf) << 2) | (i2 >>> 6);
          val o3 = i2 & 0x3F;
          out(op) = map1(o0);
          op+=1
          out(op) = map1(o1);
          op+=1
          out(op) = if (op < oDataLen) map1(o2) else '=';
          op+=1
          out(op) = if (op < oDataLen) map1(o3) else '=';
          op+=1
       }
       return out;
    }

}
