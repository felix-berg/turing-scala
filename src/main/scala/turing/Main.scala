package turing

import scala.annotation.tailrec
import TuringMachine._
object Main {
  @tailrec
  def runConfiguration[Q, A](machine: TuringMachine[Q, A], conf: Configuration[Q, A]): Configuration[Q, A] =
    if (conf.state == Accept || conf.state == Reject)
      conf
    else
      runConfiguration(machine, step(machine, conf))

  @tailrec
  def runConfiguration[Q, A](machine: MultiMachine[Q, A], conf: MultiConfig[Q, A]): MultiConfig[Q, A] =
    if (conf.state == Accept || conf.state == Reject) conf
    else runConfiguration(machine, step(machine, conf))

  def run[Q, A](machine: TuringMachine[Q, A], input: List[TapeAlph[A]]): Configuration[Q, A] =
    runConfiguration(machine, Configuration[Q, A](Nil, machine.init, Blank :: input))

  @tailrec
  def printRun[Q, A](m: TuringMachine[Q, A], conf: Configuration[Q, A], delayms: Int): Configuration[Q, A] = {
    println(conf)
    Thread.sleep(delayms)

    if (conf.state == Accept || conf.state == Reject) conf
    else printRun(m, step(m, conf), delayms)
  }

  @tailrec
  def printRun[Q, A](m: MultiMachine[Q, A], conf: MultiConfig[Q, A], delayms: Int): MultiConfig[Q, A] = {
    println(conf)
    Thread.sleep(delayms)

    if (conf.state == Accept || conf.state == Reject) conf
    else printRun(m, step(m, conf), delayms)
  }

  private def testEqual(): Unit = {
    val next = {
      var n = 0
      () => { n += 1; n }
    }

    val symbols = ('a' to 'z').toSet
    val equal = Machines.equal(symbols, (x: Char) => x.toString.toUpperCase.charAt(0), next)

    val input = List('a', 'b', 'c', 'a', 'z', 'g', '_', 'a', 'b', 'c', 'a', 'g').map {
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

  def testIncr(): Unit = {
    val next = {
      var n = 0
      () => { n += 1; n }
    }

    val incr = Machines.Binary.incr('0', '1', next)
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

    val decr = Machines.Binary.decr('0', '1', next)
    val toBin = (x: Int) => x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 2 to 10000) {
      val conf = run(decr, toBin(i)).collapse
      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == Blank :: toBin(i - 1), s"${conf.right.toString} != ${toBin(i-1)}")
    }
  }

  def randomString[A](n: Int, arr: Array[A]): List[TapeAlph[A]] =
    if (n == 0) Nil else Alph(arr(scala.util.Random.nextInt(arr.length))) :: randomString(n - 1, arr)

  def testCopy(): Unit = {
    val set = ('a' to 'z').toSet
    val arr = set.toArray

    val m = Machines.copy(set, (s: Char) => s.toString.toUpperCase.charAt(0), { var n = 0; () => { n += 1; n } })

    for (i <- 0 to 9) {
      val n = Math.pow(2, i).toInt
      val str = randomString(n, arr)
      val end = run(m, str).collapse

      assert(end.state == Accept)
      assert(end.left == Nil)
      assert(end.right == (Blank :: str) ++ (Blank :: str), s"${end.right} != ${(Blank :: str) ++ (Blank :: str)}")
    }
  }

  def testCopyDown(): Unit = {
    val set = ('a' to 'z').toSet
    val arr = set.toArray

    val m = Machines.copyDown(set, { var n = 0; () => { n += 1; n } })
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

    val m = Machines.multiEq(('a' to 'z').toSet, next)
    val x = "asdnlq".toList.map(x => Alph(x))
    var conf = MultiConfig[Int, Char](List(Nil, Nil), m.init, List(Blank :: x, Blank :: x))
    while (conf.state != Accept && conf.state != Reject) {
      println(conf)
      Thread.sleep(10)
      conf = step(m, conf)
    }
    println(conf)
  }

  def testErase(): Unit = {
    val set: Set[Char] = ('a' to 'z').toSet
    val arr = set.toArray

    val erase = Machines.erase(set, { var n = 0; () => { n += 1; n } }, '#')

    for (i <- 0 to 10) {
      val n = Math.pow(2, i).toInt
      val str = randomString(n, arr)

      val conf = run(erase, str).collapse

      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == Nil)
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
    val machine = Machines.Binary.add('0', '1', next)
    val toBin = (x: Int) =>
      if (x == 0) Nil else x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 0 to 1000) {
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
    val machine = Machines.Binary.sub('0', '1', next)
    val toBin = (x: Int) =>
      if (x == 0) Nil else x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 0 to 1000) {
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

    val max = 200
    val machine = Machines.Binary.mult('0', '1', 'Z', 'O', next)

    val toBin = (x: Int) =>
      if (x == 0) Nil else x.toBinaryString.reverse.map[TapeAlph[Char]](x => Alph(x)).to(List)

    for (i <- 0 to 5) {
      val m = scala.util.Random.nextInt(max + 1)
      val n = scala.util.Random.nextInt(max + 1 - m) + m
      assert(n >= m)

      val start = MultiConfig[Int, Char](List(Nil, Nil, Nil, Nil), machine.init, List(Blank :: toBin(n), Blank :: toBin(m), Nil, Nil))
      val end = runConfiguration(machine, start).collapse
//      val end = printRun(machine, start, 100).collapse

      assert(end.state == Accept)
      assert(end.lefts.forall(l => l == Nil))
      assert(end.rights.head == collapseTape(Blank :: toBin(n * m)), s"${end.rights.head} != ${collapseTape(Blank :: toBin(n * m))}")
    }
  }

  def dumbAdd(): Unit = {
    val next = {
      var n = 0
      () => {
        n += 1; n
      }
    }
    
    val machine = Machines.Numbers.add('1', next)
    for (i <- 0 to 12) {
      val max = Math.pow(2, i).toInt
      val m = scala.util.Random.nextInt(max)
      val n = scala.util.Random.nextInt(max)
      
      val sn = (0 until n).map(_ => Alph('1')).toList
      val sm = (0 until m).map(_ => Alph('1')).toList

      val input = Blank :: sn ++ (Blank :: sm)
      val conf = run(machine, input).collapse

      val expected = collapseTape(Blank :: (0 until (m + n)).map(_ => Alph('1')).toList)
      assert(conf.state == Accept)
      assert(conf.left == Nil)
      assert(conf.right == expected, s"${conf.right} != $expected (m = $m, n = $n)")
    }
  }

  def main(args: Array[String]): Unit = {
    val standardMethods = Set(
      "equals",
      "toString",
      "hashCode",
      "getClass",
      "notify",
      "notifyAll",
      "wait",
      "main"
    )

    this.getClass.getMethods
      .filter(f => !standardMethods.contains(f.getName))
      .filter(f => f.getParameterTypes.isEmpty)
      .foreach(f => {
        println("Test: " + f.getName)
        f.invoke(this)
      })
  }
}
