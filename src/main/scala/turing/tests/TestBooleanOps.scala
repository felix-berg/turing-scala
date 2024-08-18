package turing.tests

object TestBooleanOps {
  import turing.TuringMachine._
  import turing.TMSimulate._
  import turing.machines._
  import TestUtils._
  import scala.util.Random

  def test(): Unit = {
    val next = newNext()
    val machine = Bool.test('T', 'F', next)
    testMachine(machine, Nil, List(Blank, Alph('T')), Nil, List(Blank, Alph('T')), Accept)
    testMachine(machine, Nil, List(Blank, Alph('F')), Nil, List(Blank, Alph('F')), Reject)
  }

  def not(): Unit = {
    val next = newNext()
    val machine = Bool.not('T', 'F', next)
    testMachine(machine, Nil, List(Blank, Alph('T')), Nil, List(Blank, Alph('F')))
    testMachine(machine, Nil, List(Blank, Alph('F')), Nil, List(Blank, Alph('T')))
  }

  private def doBinOpTest(m: TuringMachine[Int, Char], a: Char, b: Char, c: Char): Unit = 
    testMachine(m, Nil, List(Blank, Alph(a), Blank, Alph(b)), Nil, List(Blank, Alph(c)))

    
  def boolOps(): Unit = {
    val next = newNext()

    {
      val machine = Bool.and('T', 'F', next)

      doBinOpTest(machine, 'T', 'T', 'T')
      doBinOpTest(machine, 'T', 'F', 'F')
      doBinOpTest(machine, 'F', 'T', 'F')
      doBinOpTest(machine, 'F', 'F', 'F')
    }
    {
      val machine = Bool.or('T', 'F', next)

      doBinOpTest(machine, 'T', 'T', 'T')
      doBinOpTest(machine, 'T', 'F', 'T')
      doBinOpTest(machine, 'F', 'T', 'T')
      doBinOpTest(machine, 'F', 'F', 'F')
    }
    {
      val table = (a: Boolean, b: Boolean) => {
        if (a) b else false // not symmetric
      }
      val machine = Bool.binOpFromTruthTable('T', 'F', table, next)
      
      doBinOpTest(machine, 'T', 'T', 'T')
      doBinOpTest(machine, 'T', 'F', 'F')
      doBinOpTest(machine, 'F', 'T', 'F')
      doBinOpTest(machine, 'F', 'F', 'F')
    }
  }

  def equality(): Unit = {
    val set = ('a' to 'z').toSet
    val alternate = ('A' to 'Z').toSet
    val machine = Bool.equality(set, alternate, '#', 'T', 'F', newNext())

    { // equal
      val x = randomTape(Random.nextInt(100) + 20, set.toArray)
      val right = Blank :: x ++ List(Blank) ++ x
      val expright = Blank :: Alph('T') :: Nil
      testMachine(machine, Nil, right, Nil, expright)
    }
    { // not equal
      val x = randomTape(Random.nextInt(100) + 20, set.toArray)
      var y = x
      while (y == x)
        y = randomTape(Random.nextInt(100) + 10, set.toArray)
      val right = Blank :: x ++ List(Blank) ++ y
      val expright = Blank :: Alph('F') :: Nil
      testMachine(machine, Nil, right, Nil, expright)
    }
  }
}
