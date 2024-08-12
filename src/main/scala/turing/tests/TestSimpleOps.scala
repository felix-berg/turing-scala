package turing.tests

object TestSimpleOps {
  import turing.TuringMachine._
  import turing.machines.SimpleOps
  import TestUtils._
  import turing.TMSimulate._
  import scala.util.Random

  private def testEqual(): Unit = {
    val next = {
      var n = 0
      () => { n += 1; n }
    }

    val symbols = ('a' to 'z').toSet
    val equal = SimpleOps.equal(symbols, (x: Char) => x.toString.toUpperCase.charAt(0), next)

    val input = "abcazg_abcag".toList.map {
      case '_' => Blank
      case x => Alph(x)
    }

    var conf = Configuration(Nil, equal.init, Blank :: input)
    while (conf.state != Accept && conf.state != Reject) {
      println(conf)
      Thread.sleep(50)
      conf = step(equal, conf)
    }
    println(conf)
  }


  def testCopy(): Unit = {
    val set = ('a' to 'z').toSet
    val arr = set.toArray

    val m = SimpleOps.copy(set, (s: Char) => s.toString.toUpperCase.charAt(0), { var n = 0; () => { n += 1; n } })

    for (i <- 0 to 9) {
      val n = Math.pow(2, i).toInt
      val str = randomString(n, arr)
      val end = run(m, str).collapse

      assert(end.state == Accept)
      assert(end.left == Nil)
      assert(end.right == (Blank :: str) ++ (Blank :: str), s"${end.right} != ${(Blank :: str) ++ (Blank :: str)}")
    }
  }

  def testErase(): Unit = {
    val set: Set[Char] = ('a' to 'z').toSet
    val arr = set.toArray

    val erase = SimpleOps.erase(set, '#', { var n = 0; () => { n += 1; n } })

    for (i <- 0 to 10) {
      val n = Math.pow(2, i).toInt
      val str = randomString(n, arr)

      val conf = run(erase, str).collapse

      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == Nil)
    }
  }

  def testNSymbols(): Unit = {
    val next = newNext()
    val n = Random.nextInt(100) + 1

    val machine = SimpleOps.nSymbols('1', n, next)
    val ns = (1 to n).map(_ => Alph('1')).toList

    testMachine(machine, Nil, Nil, Nil, Blank :: ns)

    val nothing = SimpleOps.nSymbols('1', 0, next)
    testMachine(nothing, Nil, Nil, Nil, Nil)
  }

  def writeX(): Unit = {
    val next = newNext()

    val tx = randomString(Random.nextInt(20), ('1' to 'z').toArray)
    val x = tx.map { case Alph(c) => c }
    val machine = SimpleOps.write(x, next)
    
    testMachine(machine, Nil, Nil, Nil, Blank :: tx)

    val nothing = SimpleOps.write[Char](Nil, next)
    testMachine(nothing, Nil, Nil, Nil, Nil)
  }
}
