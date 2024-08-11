package turing.tests

object TestUtils {
  import turing.TuringMachine._
  import turing.TMSimulate._

  private class DefaultNext {
    var n = -1
    def thing(): Int = {
      n += 1;
      n
    }
  }
  
  def newNext(): () => Int = {
    val n = new DefaultNext
    () => { n.thing() }
  }

  def testMachine[Q, A](machine: TuringMachine[Q, A], start: Configuration[Q, A], expected: Configuration[Q, A]): Unit = {
    val conf = runConfiguration(machine, start).collapse
    assert(conf == expected.collapse,
      s"""| testMachine failed:
          | expected = ${expected.collapse},
          | got      = $conf""".stripMargin)
  }

  def testMachine[Q, A](machine: TuringMachine[Q, A], left: List[TapeAlph[A]], right: List[TapeAlph[A]], expleft: List[TapeAlph[A]], expright: List[TapeAlph[A]], finishState: State[Q] = Accept): Unit = {
    testMachine(machine, Configuration(left, machine.init, right), Configuration(expleft, finishState, expright))
  }

  def testMachine[Q, A](machine: MultiMachine[Q, A], start: MultiConfig[Q, A], expected: MultiConfig[Q, A]): Unit = {
    val conf = runConfiguration(machine, start).collapse
    assert(conf == expected.collapse,
      s"""| testMachine failed:
          | expected = ${expected.collapse},
          | got      = $conf""".stripMargin)
  }

  def testMachine[Q, A](machine: MultiMachine[Q, A], left: List[List[TapeAlph[A]]], right: List[List[TapeAlph[A]]], expleft: List[List[TapeAlph[A]]], expright: List[List[TapeAlph[A]]], finishState: State[Q]): Unit = {
    testMachine(machine, MultiConfig(left, machine.init, right), MultiConfig(expleft, finishState, expright))
  }

  def randomStringFromTapeSymbols[A](n: Int, arr: Array[TapeAlph[A]]): List[TapeAlph[A]] = 
    if (n == 0) Nil else arr(scala.util.Random.nextInt(arr.length)) :: randomStringFromTapeSymbols(n - 1, arr)

  def randomString[A](n: Int, arr: Array[A]): List[TapeAlph[A]] =
    randomStringFromTapeSymbols(n, arr.map(s => Alph(s)))
}
