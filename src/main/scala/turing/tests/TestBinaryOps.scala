package turing.tests

object TestBinaryOps {
  import turing.machines.Binary
  import turing.TuringMachine._
  import turing.TMSimulate._
  import TestUtils._

  def testIncr(): Unit = {
    val next = {
      var n = 0
      () => { n += 1; n }
    }

    val incr = Binary.incr('0', '1', next)
    for (i <- 0 to 10000) {
      val toBin = (x: Int) => x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)
      val conf = run(incr, toBin(i))
      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == Blank :: toBin(i + 1))
    }
  }

  def testDecr(): Unit = {
    val next = {
      var n = 0
      () => { n += 1; n }
    }

    val decr = Binary.decr('0', '1', next)
    val toBin = (x: Int) => x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 2 to 10000) {
      val conf = run(decr, toBin(i)).collapse
      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == Blank :: toBin(i - 1), s"${conf.right.toString} != ${toBin(i-1)}")
    }
  }

  def testAdd(): Unit = {
    val next = {
      var n = 0
      () => {
        n += 1; n
      }
    }

    val max = 1000
    val machine = Binary.add('0', '1', next)
    val toBin = (x: Int) =>
      if (x == 0) Nil else x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 0 to 25) {
      val n = scala.util.Random.nextInt(max + 1)
      val m = scala.util.Random.nextInt(max + 1)

      val start = MultiConfig[Int, Char](List(Nil, Nil), machine.init, List(Blank :: toBin(n), Blank :: toBin(m)))
      val end = runConfiguration(machine, start).collapse

      assert(end.state == Accept)
      assert(end.lefts.forall(l => l == Nil))
      assert(end.rights.head == Blank :: toBin(n + m))
    }
  }

  def testSub(): Unit = {
    val next = {
      var n = 0
      () => {
        n += 1; n
      }
    }

    val max = 1000
    val machine = Binary.sub('0', '1', next)
    val toBin = (x: Int) =>
      if (x == 0) Nil else x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 0 to 25) {
      val m = scala.util.Random.nextInt(max + 1)
      val n = scala.util.Random.nextInt(max + 1 - m) + m
      assert(n >= m)

      val start = MultiConfig[Int, Char](List(Nil, Nil), machine.init, List(Blank :: toBin(n), Blank :: toBin(m)))
      val end = runConfiguration(machine, start).collapse

      assert(end.state == Accept)
      assert(end.lefts.forall(l => l == Nil))
      assert(end.rights.head == collapseTape(Blank :: toBin(n - m)))
    }
  }

  def testMult(): Unit = {
    val next = {
      var n = 0
      () => {
        n += 1; n
      }
    }

    val max = 25
    val machine = Binary.mult('0', '1', 'Z', 'O', next)

    val toBin = (x: Int) =>
      if (x == 0) Nil else x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 0 to 5) {
      val m = scala.util.Random.nextInt(max + 1)
      val n = scala.util.Random.nextInt(max + 1 - m) + m
      assert(n >= m)

      val start = MultiConfig[Int, Char](List(Nil, Nil, Nil, Nil), machine.init, List(Blank :: toBin(n), Blank :: toBin(m), Nil, Nil))
      val end = runConfiguration(machine, start).collapse
      // val end = printRun(machine, start, 100).collapse

      assert(end.state == Accept)
      assert(end.lefts.forall(l => l == Nil))
      assert(end.rights.head == collapseTape(Blank :: toBin(n * m)), s"${end.rights.head} != ${collapseTape(Blank :: toBin(n * m))}")
    }
  }
}
