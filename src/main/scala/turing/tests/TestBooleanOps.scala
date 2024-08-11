package turing.tests

object TestBooleanOps {
  import turing.TuringMachine._
  import turing.TMSimulate._
  import turing.machines._
  import TestUtils._

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
}
