package ChessServer.tests;

object Tester {
    import ChessServer.logic._

    var g : Game = new Game(10)

    def main(args: Array[String]): Unit = {
        test1
        test3repetitions1
        test3repetitions2
        test3repetitions3
        test50moves1
        test50moves2
    }


    def test1 = {
        runNormalTest("simple moves", List(("b2", "b3"), ("a7", "a6")), false)
    }

    def test3repetitions1 = {
        val moves = List(
            ("b1", "c3"),
            ("b8", "c6"),
            ("c3", "b1"),
            ("c6", "b8"),
            ("b1", "c3"),
            ("b8", "c6"),
            ("c3", "b1"),
            ("c6", "b8")
        )
        runNormalTest("3repetition1", moves, false)

        if (g.is3repetitions) {
            println("[PASS] 3repetition1 (valid state)")
        } else {
            println("[FAIL] 3repetition1 (draw condition not detected)")
        }

    }

    def test3repetitions2 = {
        val moves = List(
            ("a2", "a3"),
            ("a7", "a6"),
            ("a1", "a2"),
            ("a8", "a7"),
            ("a2", "a1"),
            ("a7", "a8"),
            ("a1", "a2"),
            ("a8", "a7"),
            ("a2", "a1"),
            ("a7", "a8")
        )
        runNormalTest("3repetition2", moves, false)

        if (!g.is3repetitions) {
            println("[PASS] 3repetition2 (valid state)")
        } else {
            println("[FAIL] 3repetition2 (invalid 3repetition draw condition)")
        }

    }

    def test3repetitions3 = {
        val moves = List(
            ("a2", "a3"),
            ("a7", "a6"),
            ("a1", "a2"),
            ("a8", "a7"),
            ("a2", "a1"),
            ("a7", "a8"),
            ("a1", "a2"),
            ("a8", "a7"),
            ("a2", "a1"),
            ("a7", "a8"),
            ("a1", "a2"),
            ("a8", "a7")
        )
        runNormalTest("3repetition3", moves, false)

        if (g.is3repetitions) {
            println("[PASS] 3repetition3 (valid state)")
        } else {
            println("[FAIL] 3repetition3 (draw condition not detected)")
        }

    }

    def test50moves1 = {
        val moves = List(
        ("a2", "a4"),
        ("a7", "a5"),
        ("a1", "a3"),
        ("a8", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("b1", "a3"),
        ("b8", "a6"),
        ("h3", "b3"),
        ("h6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "b3"),
        ("h6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("a3", "c4"),
        ("a6", "c5"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("c4", "e5"),
        ("c5", "e4"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("e5", "g4"),
        ("e4", "g5"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6")
        )

        runNormalTest("50moves1", moves, false)

        if (!g.is3repetitions) {
            println("[PASS] 50moves1 (valid state)")
        } else {
            println("[FAIL] 50moves1 (repetition detected)")
        }

        if (g.is50moves) {
            println("[PASS] 50moves1 (valid state)")
        } else {
            println("[FAIL] 50moves1 (draw condition not detected)")
        }

    }


    def test50moves2 = {
        val moves = List(
        ("a2", "a4"),
        ("a7", "a5"),
        ("a1", "a3"),
        ("a8", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("g2", "g4"),
        ("g7", "g5"),
        ("b1", "a3"),
        ("b8", "a6"),
        ("h3", "b3"),
        ("h6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "b3"),
        ("h6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("a3", "c4"),
        ("a6", "c5"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("c4", "e5"),
        ("c5", "e4"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6"),
        ("h3", "a3"),
        ("h6", "a6"),
        ("a3", "b3"),
        ("a6", "b6"),
        ("b3", "c3"),
        ("b6", "c6"),
        ("c3", "d3"),
        ("c6", "d6"),
        ("d3", "e3"),
        ("d6", "e6"),
        ("e3", "f3"),
        ("e6", "f6"),
        ("f3", "g3"),
        ("f6", "g6"),
        ("g3", "h3"),
        ("g6", "h6")
        )

        runNormalTest("50moves2", moves, false)


        if (!g.is3repetitions) {
            println("[PASS] 50moves2 (valid state)")
        } else {
            println("[FAIL] 50moves2 (repetition detected)")
        }

        if (!g.is50moves) {
            println("[PASS] 50moves2 (valid state)")
        } else {
            println("[FAIL] 50moves2 (draw condition wrongly detected)")
        }

    }


    def runNormalTest(title: String, moves: List[(String,String)], expectedExc: Boolean) = {
        runTest(moves, expectedExc) match {
            case Some(error) => println("[FAIL] "+title); println("     -> "+error)
            case None => println("[PASS] "+title)
        }
    }
    def runTest(moves: List[(String,String)], expectedExc: Boolean): Option[String] = {
        var foundExc : Option[Exception] = None

        try {
            g = new Game(10).start;
            for (m <- moves) {
                try {
                    g = g.move(Position(m._1), Position(m._2))
                    //println("Move "+m+" ok")
                } catch {
                    case e: Exception =>
                        throw new Exception("(move="+m+"):"+e.toString)
                }
            }
        } catch {
            case e: Exception =>
                foundExc = Some(e)
        }

        (expectedExc, foundExc) match {
            case (true, Some(e)) => None
            case (false, Some(e)) => Some("Unexpected exception: "+e.getMessage)
            case (true, None) => Some("Expected exception!")
            case (false, None) => None
        }
    }
}
