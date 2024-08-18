package turing.tests

object TestSimpleOps {
  import turing.TuringMachine._
  import turing.machines.SimpleOps
  import TestUtils._
  import turing.TMSimulate._
  import scala.util.Random

  def testEqual(): Unit = {
    val next = newNext()
    val set = ('a' to 'z').toSet
    val alternate = ('A' to 'Z').toSet
    val setarr = set.toArray
    val machine = SimpleOps.equal(set, set.zip(alternate).toMap, next)

    for (i <- 1 to 10) {
      val x = randomTape(Random.nextInt(100), setarr)
      { // equal
        val right = Blank :: x ++ List(Blank) ++ x
        testMachine(machine, Nil, right, Nil, right, Accept)
      }
      { // |x| = |y|, x != y
        var y = x
        while (y == x) y = x.map(_ => Alph(setarr(Random.nextInt(set.size))))
        val right = Blank :: x ++ List(Blank) ++ y
        testMachine(machine, Nil, right, Nil, right, Reject)
      }
      { // |y| > |x|, y = xz (for some z, |z| > 0)
        val y = x ++ randomTape(Random.nextInt(10) + 1, setarr)
        val right = Blank :: x ++ List(Blank) ++ y
        testMachine(machine, Nil, right, Nil, right, Reject)
      }
      { // |y| < |x|, x = yz
        val y = x.take(Random.between(1, x.length))
        val right = Blank :: x ++ List(Blank) ++ y
        testMachine(machine, Nil, right, Nil, right, Reject)
      }
    }
    { // length 0
      testMachine(machine, Nil, Nil, Nil, Nil, Accept)
    }
  }


  def testCopy(): Unit = {
    val set = ('a' to 'z').toSet
    val arr = set.toArray

    val m = SimpleOps.copy(set, (s: Char) => s.toString.toUpperCase.charAt(0), { var n = 0; () => { n += 1; n } })

    for (i <- 0 to 9) {
      val n = Math.pow(2, i).toInt
      val str = randomTape(n, arr)
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
      val str = randomTape(n, arr)

      val conf = run(erase, str).collapse

      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == Nil)
    }
  }

  def testEraseN(): Unit = {
    val set: Set[Char] = ('1' to 'z').toSet
    val arr = set.toArray

    val n = Random.nextInt(20) + 5
    val erase = SimpleOps.eraseN(n, set, '#', newNext())
    val words = (1 to n).map(_ => Blank :: randomTape(Random.nextInt(10), arr)).toList.flatten
    val garbage = (1 to Random.nextInt(100)).map(_ => randomTape(Random.nextInt(10) + 1, arr) ++ List(Blank)).flatten.toList
    
    val right = words ++ List(Blank) ++ garbage
    val expright = words.map(_ => Blank) ++ List(Blank) ++ garbage
    testMachine(erase, Nil, right, Nil, expright)

    val nothing = SimpleOps.eraseN(0, set, '#', newNext())
    testMachine(nothing, Nil, Blank :: garbage, Nil, Blank :: garbage)

    val word = randomTape(Random.nextInt(10), arr)
    val one = SimpleOps.eraseN(1, set, '#', newNext())
    testMachine(one, Nil, Blank :: word ++ List(Blank) ++ garbage, Nil, Blank :: word.map(_ => Blank) ++ List(Blank) ++ garbage)
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

    val tx = randomTape(Random.nextInt(20), ('1' to 'z').toArray)
    val x = tx.map { case Alph(c) => c }
    val machine = SimpleOps.write(x, next)
    
    testMachine(machine, Nil, Nil, Nil, Blank :: tx)

    val nothing = SimpleOps.write[Char](Nil, next)
    testMachine(nothing, Nil, Nil, Nil, Nil)
  }
}
