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
    (1 to n).map(_ => '1').mkString

  private def testProgram(prog: String, left: String, right: String, expleft: String, expright: String): Unit = {
    val next = newNext()
    val ast = miniscala.parser.Parser.parse(prog)
    val m = compile(ast, next)

    testMachine(m, toTape(left).reverse, toTape(right), toTape(expleft.reverse), toTape(expright))
  }

  private def simProgram(prog: String, left: String = "", right: String = ""): Unit = {
    val next = newNext()
    val ast = miniscala.parser.Parser.parse(prog)
    val m = compile(ast, next)
    
    printRunConfiguration(m, Configuration(toTape(left).reverse, m.init, toTape(right)), 100)
  }

  def constants(): Unit = {
    testProgram("1", "", "", "", "_1")
    testProgram("2", "", "", "", "_11")
    testProgram("11", "", "", "", "_" + unaryString(11))
    testProgram("true", "", "", "", "_T")
    testProgram("false", "", "", "", "_F")
  }

  def addition(): Unit = {
    testProgram("0 + 5", "", "", "", "_11111")
    testProgram("5 + 0", "", "", "", "_11111")
    testProgram("0 + 0", "", "", "", "")

    for (i <- 1 to 100) {
      val m = Random.nextInt(100)
      val n = Random.nextInt(100) + m
      val mns = '_' + (1 to m + n).map(_ => '1').mkString
      testProgram(s"$m + $n",  "", "", "", mns)
    }
  }

  def subtraction(): Unit = {
    testProgram("5 - 0", "", "", "", "_11111")
    testProgram("0 - 0", "", "", "", "")

    for (i <- 1 to 100) {
      val n = Random.nextInt(100)
      val m = Random.nextInt(100) + n
      assert(m >= n)
      val mns = '_' + (1 to m - n).map(_ => '1').mkString
      testProgram(s"$m - $n",  "", "", "", mns)
    }
  }
  
  def multiplication(): Unit = {
    testProgram("5 * 0", "", "", "", "")
    testProgram("0 * 5", "", "", "", "")
    testProgram("0 * 0", "", "", "", "")

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
    testProgram("true  & true",  "", "", "", s"_$t")
    testProgram("true  & false", "", "", "", s"_$f")
    testProgram("false & true",  "", "", "", s"_$f")
    testProgram("false & false", "", "", "", s"_$f")

    testProgram("true  | true",  "", "", "", s"_$t")
    testProgram("true  | false", "", "", "", s"_$t")
    testProgram("false | true",  "", "", "", s"_$t")
    testProgram("false | false", "", "", "", s"_$f")
  }

  def ifThenElse(): Unit = {
    testProgram("if (true) 1 else 2", "", "", "", s"_1")
    testProgram("if (false) 1 else 2", "", "", "", s"_11")
    testProgram("if (false) true else false", "", "", "", s"_$FALSE")
  }
}
