package turing.tests

import turing.TMSimulate.printRunConfiguration
import turing.TMSimulate

object TestUnaryMulitply {
  import turing.machines._
  import turing.TuringMachine._
  import scala.util.Random
  import TestUtils._

  def testDelBack(): Unit = {
    val machine = Unary.MulImpl.delBack('1', newNext())
    val ones = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val garbage = randomTape(102, ('1' to 'z').toArray)

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

    val machine = Unary.MulImpl.init('1', '#', next)
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

    val machine = Unary.MulImpl.emptyBehind('1', '#', next)
    val garbage = randomTape(Random.nextInt(100), ('1' to 'z').toArray)
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
    val machine = Unary.MulImpl.appendSpecial(symbols, '#', next)
    
    val garb1 = randomTape(Random.nextInt(100), (symbols + '#').toArray)
    val word = randomTape(Random.nextInt(100), symbols.toArray)
    val garb2 = randomTape(Random.nextInt(100), (symbols + '#').toArray)

    val left = garb1
    val right = Blank :: word ++ List(Blank) ++ garb2

    val expleft = garb1
    val expright = Blank :: word ++ (Alph('#') :: garb2)
    
    testMachine(machine, left, right, expleft, expright)
  }

  def testFind(): Unit = {
    val next = newNext()
    val symbols = (0.toChar to 255.toChar).toSet - '#'
    val machine = Unary.MulImpl.find(symbols + '#', '#', next)
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
    val machine = Unary.MulImpl.copyAcross('1', '#', 'A', next)

    val garbage = randomTape(Random.nextInt(100) + 1, ('1' to 'z').toArray)
    val ones = randomTape(Random.nextInt(100) + 1, Array('1'))

    val left = (garbage ++ List(Blank) ++ ones).reverse
    val right = List(Alph('#'))

    val expleft = left
    val expright = List(Alph('#')) ++ ones
  }

  def testMulIterate(): Unit = {
    val next = newNext()

    val machine = Unary.MulImpl.mulIterate('1', '#', 'A', next)
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

    val machine = Unary.MulImpl.combineGroups('1', '#', next)
    val ones = (0 to Random.nextInt(100) + 5).map(_ => Alph('1')).toList

    val nGroups = 5 + Random.nextInt(100)
    val spacedGroups = (1 to nGroups).map(_ => Blank :: ones).toList.flatten.tail
    val garbage = randomTape(Random.nextInt(100) + 5, ('1' to 'z').toArray)
    

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
    val machine = Unary.MulImpl.trimLeft('1', '#', next)

    val blanks = (0 to Random.nextInt(100)).map(_ => Blank).toList
    val ones = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
    val garbage = randomTape(Random.nextInt(100), ('1' to 'z').toArray)

    val left = (Alph('#') :: blanks).reverse
    val right = ((Blank :: ones) ++ List(Alph('#'))) ++ garbage

    val expleft = Nil
    val expright = (Alph('#') :: ones) ++ List(Alph('#')) ++ (Blank :: blanks) ++ garbage

    testMachine(machine, left, right, expleft, expright)
  }

  def testLastBlank(): Unit = {
    val next = newNext()
    val machine = Unary.MulImpl.lastBlank(Set('1'), next)
    val garbage = randomTape(Random.nextInt(100), ('2' to 'z').toArray)
    val blanks = (0 to Random.nextInt(100)).map(_ => Blank).toList
    val left = Nil
    val right = blanks ++ List(Alph('1')) ++ garbage
    val expleft = blanks.tail
    val expright = Blank :: List(Alph('1')) ++ garbage

    testMachine(machine, left, right, expleft, expright)
  }

  private def testMulFinish(): Unit = {
    // FIXME: flaky
    val next = newNext()
    val machine = Unary.MulImpl.mulFinish('1', '#', next)
    for (i <- 1 to 10) {
      val m = Random.nextInt(20) + 1
      val n = Random.nextInt(20) + 1

      val group = (1 to n).map(_ => Alph('1')).toList
      val groups = (1 to m).map(_ => Blank :: group).toList.flatten.tail
      val blanks = (0 to Random.nextInt(10)).map(_ => Blank).toList

      val left = List(Alph('#'))
      val right = blanks ++ groups ++ List(Alph('#'))

      val expleft = Nil
      val expright = Blank :: (1 to m * n).map(_ => Alph('1')).toList

      testMachine(machine, left, right, expleft, expright)
    }
  }

  def testPositiveMul(): Unit = {
    val next = newNext()
    val machine = Unary.MulImpl.positiveMultiply('1', '#', 'A', next)
    
    for (i <- 1 to 200) {
      val m = Random.nextInt(25) + 1
      val n = Random.nextInt(25) + 1

      val right = List(Blank) ++ (1 to m).map(_ => Alph('1')).toList ++ List(Blank) ++ (1 to n).map(_ => Alph('1')).toList
      val expright = List(Blank) ++ (1 to m * n).map(_ => Alph('1')).toList

      testMachine(machine, Nil, right, Nil, expright)
    }
  }

  def testCleanAndRejectIfZero(): Unit = {
    val next = newNext()
    val machine = Unary.MulImpl.cleanAndRejectIfZero('1', '#', next)

    { 
      // m = 0, n > 0
      val ms = Nil
      val ns = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList

      val right = (Blank :: ms) ++ List(Blank) ++ ns
      val expright = Nil

      testMachine(machine, Nil, right, Nil, Nil, Reject)

      // m > 0, n = 0
      val right2 = (Blank :: ns) ++ List(Blank) ++ ms
      val expright2 = Nil
      testMachine(machine, Nil, right2, Nil, Nil, Reject)

      // m = 0, n = 0
      testMachine(machine, Nil, Nil, Nil, Nil, Reject)
    }

    { // m, n > 0
      val ms = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList
      val ns = (0 to Random.nextInt(100)).map(_ => Alph('1')).toList

      val right = Blank :: ms ++ List(Blank) ++ ns
      testMachine(machine, Nil, right, Nil, right, Accept)
    }
  }

  def testMul(): Unit = {
    val next = newNext()
    val machine = Unary.mul('1', '#', 'A', next)

    def doTest(m: Int, n: Int): Unit = {
      val ms = (1 to m).map(_ => Alph('1')).toList
      val ns = (1 to n).map(_ => Alph('1')).toList
      val mns = (1 to m * n).map(_ => Alph('1')).toList
      val right = Blank :: ms ++ List(Blank) ++ ns
      val expright = Blank :: mns

      testMachine(machine, Nil, right, Nil, expright)
    }

    doTest(0, Random.nextInt(20))
    doTest(Random.nextInt(20), 0)
    doTest(0, 0)
    for (i <- 1 to 100) {
      val m = Random.nextInt(20)
      val n = Random.nextInt(20)
      doTest(m, n)
    }
  }
}
