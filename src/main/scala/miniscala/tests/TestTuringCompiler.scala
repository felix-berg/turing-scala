package miniscala.tests

object TestTuringCompiler {
  import turing.TMSimulate._
  import miniscala.Ast._
  import miniscala.TuringCompiler._
  import turing.tests.TestUtils._
  import turing.TuringMachine._
  import scala.util.Random

  private def toTape(s: String): List[TapeAlph[Char]] = 
    s.map(c => c match {
      case '_' => Blank
      case c => Alph(c)
    }).toList

  private def unaryString(n: Int): String =
    (1 to n).map(_ => ONE).mkString

  private def testProgram(prog: String, left: String, right: String, expleft: String, expright: String): Unit = {
    val next = newNext()
    val ast = miniscala.parser.Parser.parse(prog)
    val m = compile(ast, next)

    testMachine(m, List(toTape(left).reverse, List(Alph(HASH)), Nil), List(toTape(right), Nil, Nil), List(toTape(expleft).reverse, List(Alph(HASH)), Nil), List(toTape(expright), Nil, Nil), Accept)
  }

  private def testProgram(prog: String, expright: String): Unit = {
    testProgram(prog, "", "", "", expright)
  }

  private def simProgram(prog: String, left: String = "", right: String = ""): Unit = {
    val next = newNext()
    val ast = miniscala.parser.Parser.parse(prog)
    val m = compile(ast, next)
    
    printRunConfiguration(m, MultiConfig(List(toTape(left).reverse, List(Alph(HASH)), Nil), m.init, List(toTape(right), Nil, Nil)), 50)
  }

  def constants(): Unit = {
    testProgram("1", "_1")
    testProgram("2", "_11")
    testProgram("11", "_" + unaryString(11))
    testProgram("true", "_T")
    testProgram("false", "_F")
  }

  def addition(): Unit = {
    testProgram("0 + 5", "_11111")
    testProgram("5 + 0", "_11111")
    testProgram("0 + 0", "")

    for (i <- 1 to 100) {
      val m = Random.nextInt(100)
      val n = Random.nextInt(100) + m
      val mns = '_' + (1 to m + n).map(_ => '1').mkString
      testProgram(s"$m + $n",  "", "", "", mns)
    }
  }

  def subtraction(): Unit = {
    testProgram("5 - 0", "_11111")
    testProgram("0 - 0", "")

    for (i <- 1 to 100) {
      val n = Random.nextInt(100)
      val m = Random.nextInt(100) + n
      assert(m >= n)
      val mns = '_' + (1 to m - n).map(_ => '1').mkString
      testProgram(s"$m - $n",  "", "", "", mns)
    }
  }
  
  def multiplication(): Unit = {
    testProgram("5 * 0", "")
    testProgram("0 * 5", "")
    testProgram("0 * 0", "")

    for (i <- 1 to 20) {
      val m = Random.nextInt(20)
      val n = Random.nextInt(20)
      val mns = '_' + (1 to m * n).map(_ => '1').mkString
      testProgram(s"$m * $n",  "", "", "", mns)
    }
  }

  def booleanOps(): Unit = {
    val t = TRUE
    val f = FALSE
    testProgram("true  & true",  s"_$t")
    testProgram("true  & false", s"_$f")
    testProgram("false & true",  s"_$f")
    testProgram("false & false", s"_$f")

    testProgram("true  | true",  s"_$t")
    testProgram("true  | false", s"_$t")
    testProgram("false | true",  s"_$t")
    testProgram("false | false", s"_$f")

    testProgram("!true", s"_$f")
    testProgram("!false", s"_$t")
    testProgram("!(!true)", s"_$t")
    testProgram("!(!false)", s"_$f")
  }

  def equality(): Unit = {
    testProgram("1 == 1", s"_$TRUE")
    testProgram("1 == 2", s"_$FALSE")
    testProgram("true == true", s"_$TRUE")
    testProgram("true == false", s"_$FALSE")
    testProgram("false == false", s"_$TRUE")
    testProgram("false == true", s"_$FALSE")
  }

  def ifThenElse(): Unit = {
    testProgram("if (true) 1 else 2", s"_1")
    testProgram("if (false) 1 else 2", s"_11")
    testProgram("if (false) true else false", s"_$FALSE")
  }

  def valDecls(): Unit = {
    testProgram("{ val x = 1; 1 }", "_1")
    testProgram("{ val x = 2; x }", "_11")
    testProgram("{ val x = 2; { val x = 4; x } + x }", "_111111")
    testProgram("{ val x = 2; { val y = 5; val x = 4; x + y } + x }", "_11111111111")
  }

  def combinations(): Unit = {
    testProgram(
      """{ val x = 10;
         | val y = x * 2;
         | val z = if (y == x * 2) y * x else x - 2;
         | y + z }""".stripMargin, 
         "_" + (1 to 220).map(_ => '1').mkString
    )
  }

  def lambdas(): Unit = {
    val zeroCode = (1 to CODELENGTH).map(_ => CODEZERO).mkString
    testProgram("(x, y) => 1 + 2", s"_$zeroCode;;x,y")
    simProgram("((x, y) => x + y)(1, 2)")
  }
}
