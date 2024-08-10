package turing

import scala.annotation.tailrec
import TuringMachine._
import scala.util.Random
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
  def printRunConfiguration[Q, A](m: TuringMachine[Q, A], conf: Configuration[Q, A], delayms: Int): Configuration[Q, A] = {
    println(conf)
    Thread.sleep(delayms)

    if (conf.state == Accept || conf.state == Reject) conf
    else printRunConfiguration(m, step(m, conf), delayms)
  }

  @tailrec
  def printRunConfiguration[Q, A](m: MultiMachine[Q, A], conf: MultiConfig[Q, A], delayms: Int): MultiConfig[Q, A] = {
    println(conf)
    Thread.sleep(delayms)

    if (conf.state == Accept || conf.state == Reject) conf
    else printRunConfiguration(m, step(m, conf), delayms)
  }

  def printRun[Q, A](m: TuringMachine[Q, A], input: List[TapeAlph[A]], delayms: Int): Configuration[Q, A] =
    printRunConfiguration(m, Configuration(Nil, m.init, Blank :: input), delayms)

  class DefaultNext {
    var n = -1
    def thing(): Int = {
      n += 1;
      n
    }
  }
  
  private def newNext(): () => Int = {
    val n = new DefaultNext
    () => { n.thing() }
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

  def randomStringFromTapeSymbols[A](n: Int, arr: Array[TapeAlph[A]]): List[TapeAlph[A]] = 
    if (n == 0) Nil else arr(scala.util.Random.nextInt(arr.length)) :: randomStringFromTapeSymbols(n - 1, arr)

  def randomString[A](n: Int, arr: Array[A]): List[TapeAlph[A]] =
    randomStringFromTapeSymbols(n, arr.map(s => Alph(s)))

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
      // val end = printRun(machine, start, 100).collapse

      assert(end.state == Accept)
      assert(end.lefts.forall(l => l == Nil))
      assert(end.rights.head == collapseTape(Blank :: toBin(n * m)), s"${end.rights.head} != ${collapseTape(Blank :: toBin(n * m))}")
    }
  }

  def dumbAdd(): Unit = {
    val next = {
      var n = -1
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
      val bconf = Configuration(Nil, machine.init, input)
      val conf = runConfiguration(machine, bconf).collapse

      val expected = collapseTape(Blank :: (0 until (m + n)).map(_ => Alph('1')).toList)
      assert(conf.state == Accept, s"non-accepted state")
      assert(conf.left == Nil, s"left is non-empty")
      assert(conf.right == expected, s"${conf.right} != $expected (m = $m, n = $n, input = $input)")
    }
  }

  def dumbSub(): Unit = {
    val next = {
      var n = -1
      () => {
        n += 1; n
      }
    }

    
    val machine = Machines.Numbers.sub('1', '#', next)

    for (i <- 0 to 12) {
      val max = Math.pow(2, i).toInt
      val n = scala.util.Random.nextInt(max)
      val m = scala.util.Random.nextInt(max - n) + n
      assert(m >= n, s"bug in test,bug in test,  $m < $n")
      
      val sn = (0 until n).map(_ => Alph('1')).toList
      val sm = (0 until m).map(_ => Alph('1')).toList

      val input = Blank :: sm ++ (Blank :: sn)
      val bconf = Configuration(Nil, machine.init, input)
      val conf = runConfiguration(machine, bconf).collapse

      val expected = collapseTape(Blank :: (0 until (m - n)).map(_ => Alph('1')).toList)
      assert(conf.state == Accept, s"non-accepted state")
      assert(conf.left == Nil, s"left is non-empty")
      assert(conf.right == expected, s"${conf.right} != $expected (m = $m, n = $n, input = $input)")
    }
  }

  private def testMachine[Q, A](machine: TuringMachine[Q, A], start: Configuration[Q, A], expected: Configuration[Q, A]): Unit = {
    val conf = runConfiguration(machine, start).collapse
    assert(conf == expected.collapse,
      s"""| testMachine failed:
          | expected = ${expected.collapse},
          | got      = $conf""".stripMargin)
  }

  private def testMachine[Q, A](machine: TuringMachine[Q, A], left: List[TapeAlph[A]], right: List[TapeAlph[A]], expleft: List[TapeAlph[A]], expright: List[TapeAlph[A]], finishState: State[Q] = Accept): Unit = {
    testMachine(machine, Configuration(left, machine.init, right), Configuration(expleft, finishState, expright))
  }

  def testDelBack(): Unit = {
    val machine = Machines.Numbers.MulImpl.delBack('1', newNext())
    val ones = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val garbage = randomString(102, ('1' to 'z').toArray)

    // #1111111Δgarbage
    //         ^
    val left = (Alph('#') :: ones).reverse
    // #111111ΔΔgarbage
    //        ^
    val right = Blank :: garbage

    val expleft = (Alph('#') :: ones.take(ones.length - 1)).reverse
    val expright = Blank :: Blank :: garbage

    testMachine(machine, left, right, expleft, expright)
  }

  def testInit(): Unit = {
    val next = newNext()

    val machine = Machines.Numbers.MulImpl.init('1', '#', next)
    val ones1 = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val ones2 = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList

    val left = Nil 
    val right = (Blank :: ones1) ++ List(Blank) ++ ones2

    val expleft = (Alph('#') :: ones1.tail).reverse
    val expright = List(Blank, Blank) ++ ones2 ++ List(Alph('#'))

    testMachine(machine, left, right, expleft, expright)
  }


  def testEmptyBehind(): Unit = {
    val next = newNext()

    val machine = Machines.Numbers.MulImpl.emptyBehind('1', '#', next)
    val garbage = randomString(Random.nextInt(100), ('1' to 'z').toArray)
    val blanks = (0 to Random.nextInt(50)).map(_ => Blank).toList

    { // false case
      val ones = (0 to Random.nextInt(50)).map(_ => Alph('1')).toList
      val left = (Alph('#') :: ones).reverse
      val right = blanks ++ garbage
      testMachine(machine, left, right, left, right, Reject)
    }
    { // true case
      val left = List(Alph('#'))
      val right = blanks ++ garbage
      testMachine(machine, left, right, left, right, Accept)
    }
  }

  def testAppendSpecial(): Unit = {
    val next = newNext()
    val symbols = (0.toChar to 255.toChar).toSet - '#'
    val machine = Machines.Numbers.MulImpl.appendSpecial(symbols, '#', next)
    
    val garb1 = randomString(Random.nextInt(100), (symbols + '#').toArray)
    val word = randomString(Random.nextInt(100), symbols.toArray)
    val garb2 = randomString(Random.nextInt(100), (symbols + '#').toArray)

    val left = garb1
    val right = Blank :: word ++ List(Blank) ++ garb2

    val expleft = garb1
    val expright = Blank :: word ++ (Alph('#') :: garb2)
    
    testMachine(machine, left, right, expleft, expright)
  }

  def testFind(): Unit = {
    val next = newNext()
    val symbols = (0.toChar to 255.toChar).toSet - '#'
    val machine = Machines.Numbers.MulImpl.find(symbols + '#', '#', next)
    val left = randomStringFromTapeSymbols(Random.nextInt(40), (symbols.map(s => Alph(s)) + Blank).toArray)
    val r1 = randomStringFromTapeSymbols(Random.nextInt(40), (symbols.map(s => Alph(s)) + Blank).toArray)
    val r2 = randomStringFromTapeSymbols(Random.nextInt(40), (symbols.map(s => Alph(s)) + Blank).toArray)
    val right = r1 ++ List(Alph('#')) ++ r2

    val expleft = r1.reverse ++ left
    val expright = Alph('#') :: r2
    
    testMachine(machine, left, right, expleft, expright)
  }

  def testCopyAcross(): Unit = {
    val next = newNext()
    val machine = Machines.Numbers.MulImpl.copyAcross('1', '#', 'A', next)

    val garbage = randomString(Random.nextInt(100) + 1, ('1' to 'z').toArray)
    val ones = randomString(Random.nextInt(100) + 1, Array('1'))

    val left = (garbage ++ List(Blank) ++ ones).reverse
    val right = List(Alph('#'))

    val expleft = left
    val expright = List(Alph('#')) ++ ones
  }

  def testMulIterate(): Unit = {
    val next = newNext()

    val machine = Machines.Numbers.MulImpl.mulIterate('1', '#', 'A', next)
    val ones = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val oneGroups = (0 to Random.nextInt(100)).map(_ => Blank :: ones).toList.flatten

    val muls = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val blanks = (0 to Random.nextInt(100)).map(_ => Blank).toList

    val left = (Alph('#') :: muls).reverse
    val right = blanks ++ oneGroups ++ List(Alph('#'))
    val expleft = left.tail
    val expright = (Blank :: blanks) ++ oneGroups ++ List(Blank) ++ ones ++ List(Alph('#'))

    testMachine(machine, left, right, expleft, expright)
  }

  def testCombineGroups(): Unit = {
    val next = newNext()

    val machine = Machines.Numbers.MulImpl.combineGroups('1', '#', next)
    val ones = (0 to Random.nextInt(100) + 5).map(_ => Alph('1')).toList

    val nGroups = 5 + Random.nextInt(100)
    val spacedGroups = (1 to nGroups).map(_ => Blank :: ones).toList.flatten.tail
    val garbage = randomString(Random.nextInt(100) + 5, ('1' to 'z').toArray)
    

    val nonSpacedGroups = (1 to nGroups).map(_ => ones).toList.flatten
    val blanks = (1 to nGroups - 1).map(_ => Blank).toList

    val left = Nil
    val right = (Alph('#') :: spacedGroups) ++ List(Alph('#')) ++ garbage

    val expleft = Nil
    val expright = (Alph('#') :: nonSpacedGroups) ++ List(Alph('#')) ++ blanks ++ garbage

    testMachine(machine, left, right, expleft, expright)
  }

  def testTrimLeft(): Unit = {
    val next = newNext()
    val machine = Machines.Numbers.MulImpl.trimLeft('1', '#', next)

    val blanks = (0 to Random.nextInt(100)).map(_ => Blank).toList
    val ones = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val garbage = randomString(Random.nextInt(100), ('1' to 'z').toArray)

    val left = (Alph('#') :: blanks).reverse
    val right = ((Blank :: ones) ++ List(Alph('#'))) ++ garbage

    val expleft = Nil
    val expright = (Alph('#') :: ones) ++ List(Alph('#')) ++ (Blank :: blanks) ++ garbage

    testMachine(machine, left, right, expleft, expright)
  }

  def testLastBlank(): Unit = {
    val next = newNext()
    val machine = Machines.Numbers.MulImpl.lastBlank(Set('1'), next)
    val garbage = randomString(Random.nextInt(100), ('2' to 'z').toArray)
    val blanks = (0 to Random.nextInt(100)).map(_ => Blank).toList
    val left = Nil
    val right = blanks ++ List(Alph('1')) ++ garbage
    val expleft = blanks.tail
    val expright = Blank :: List(Alph('1')) ++ garbage

    testMachine(machine, left, right, expleft, expright)
  }

  def testMulFinish(): Unit = {
    val next = newNext()
    val m = Random.nextInt(100) + 1
    val n = Random.nextInt(100) + 1
  
    val machine = Machines.Numbers.MulImpl.mulFinish('1', '#', next)
    val group = (1 to n).map(_ => Alph('1')).toList
    val groups = (1 to m).map(_ => Blank :: group).toList.flatten.tail
    val blanks = (0 to Random.nextInt(10)).map(_ => Blank).toList

    val left = List(Alph('#'))
    val right = blanks ++ groups ++ List(Alph('#'))

    val expleft = Nil
    val expright = Blank :: (1 to m * n).map(_ => Alph('1')).toList

    testMachine(machine, left, right, expleft, expright)
  }

  def testPositiveMul(): Unit = {
    val next = newNext()
    val machine = Machines.Numbers.MulImpl.positiveMultiply('1', '#', 'A', next)
    
    for (i <- 1 to 200) {
      val m = Random.nextInt(25) + 1
      val n = Random.nextInt(25) + 1

      val right = List(Blank) ++ (1 to m).map(_ => Alph('1')).toList ++ List(Blank) ++ (1 to n).map(_ => Alph('1')).toList
      val expright = List(Blank) ++ (1 to m * n).map(_ => Alph('1')).toList

      testMachine(machine, Nil, right, Nil, expright)
    }
  }

  def main(args: Array[String]): Unit = {
    Random.setSeed(System.currentTimeMillis)

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
