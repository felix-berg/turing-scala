package turing.tests

object TestMultiTapeOps {
  import turing.TuringMachine._
  import turing.machines.MultiTapeOps
  import turing.tests.TestUtils._
  import turing.TMSimulate._

  def testCopyDown(): Unit = {
    val set = ('a' to 'z').toSet
    val arr = set.toArray

    val m = MultiTapeOps.copyDown(set, { var n = 0; () => { n += 1; n } })
    for (i <- 0 to 10) {
      val str = randomString(Math.pow(2, i).toInt, arr)

      val start = MultiConfig(
        List(Nil, Nil),
        m.init,
        List(Blank :: str, Nil)
      )

      val end = runConfiguration(m, start).collapse
      assert(end.lefts.forall(l => l == Nil))
      assert(end.state == Accept)
      assert(end.rights.forall(r => r == Blank :: str), s"Expected $str got ${end}")
    }
  }

  private def testMulti(): Unit = {
    val init = NonHalt(0)
    val m: MultiMachine[Int, Char] = MultiMachine(NonHalt(0), Map(
      (init, List(Blank, Blank)) -> (init, List('0', '1').map(c => Alph(c)), List(Right, Right))
    ))

    var conf = MultiConfig[Int, Char](List(Nil, Nil), init, List(Nil, Nil))
    for (i <- 0 to 100) {
      println(conf)
      conf = step(m, conf)
    }
    println(conf)
  }

  private def testMultiEq(): Unit = {
    val next = {
      var n = 0
      () => { n += 1; n }
    }

    val m = MultiTapeOps.multiEq(('a' to 'z').toSet, next)
    val x = "asdnlq".toList.map(x => Alph(x))
    var conf = MultiConfig[Int, Char](List(Nil, Nil), m.init, List(Blank :: x, Blank :: x))
    while (conf.state != Accept && conf.state != Reject) {
      println(conf)
      Thread.sleep(10)
      conf = step(m, conf)
    }
    println(conf)
  }
}
