package turing.tests

import turing.TMSimulate.printRunConfiguration

object TestEnvStack {
  import TestUtils._
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.machines.EnvStack
  import scala.util.Random
  
  private def toTape(s: String): List[TapeAlph[Char]] = 
    s.map(c => c match {
      case '_' => Blank
      case c => Alph(c)
    }).toList

  private def unaryString(n: Int): String =
    (1 to n).map(_ => '1').mkString

  def push(): Unit = {
    val next = newNext()
    val machine = EnvStack.push(('a' to 'z').toSet, Set('1'), '#', next)
    assert(valid(machine) && numTapes(machine) == 2)
      
    val name = randomTape(Random.nextInt(20) + 1, ('a' to 'z').toArray)
    val ones = toTape(unaryString(Random.nextInt(20) + 1))
    val garbage = randomTape(Random.nextInt(100), ('1' to 'z').toArray)
    
    val left = List(Nil, Nil)
    val right = List(Blank :: name ++ List(Blank) ++ ones ++ List(Blank) ++ garbage, Nil)

    val expleft = List(Nil, (Blank :: name ++ List(Blank) ++ ones).reverse)
    val expright = List((1 to name.length + ones.length + 3).map(_ => Blank).toList ++ garbage, Nil)

    testMachine(machine, left, right, expleft, expright, Accept)
  }

  def pop(): Unit = {
    val next = newNext()
    val machine = EnvStack.pop(('a' to 'z').toSet, Set('1'), next)
    assert(valid(machine) && numTapes(machine) == 2)

    val name = randomTape(Random.nextInt(20) + 1, ('a' to 'z').toArray)
    val ones = toTape(unaryString(Random.nextInt(20) + 1))

    val left = List(Nil, (Blank :: name ++ List(Blank) ++ ones).reverse)
    val right = List(Nil, Nil)
    val expleft = List(Nil, Nil)
    val expright = List(Nil, Nil)

    testMachine(machine, left, right, expleft, expright, Accept)
  }

  def getValueIfMatches(): Unit = {
    val next = newNext()
    val namesymbols = ('a' to 'z').toSet
    val valuesymbols = Set('1')
    val machine = EnvStack.Impl.getValueIfMatches(namesymbols, valuesymbols, '#', next)
    
    { // good case
      val x = randomTape(Random.nextInt(20) + 1, namesymbols.toArray)
      val v = toTape(unaryString(Random.nextInt(20) + 1))
      val pair = Blank :: x ++ List(Blank) ++ v
      
      val left = List(Nil, Nil)
      val right = List(Blank :: x, pair)
      val expleft = List(Nil, Nil)
      val expright = List(Blank :: v, pair)
      
      testMachine(machine, left, right, expleft, expright, Accept)
    }
    { // bad case
      val x = randomTape(Random.nextInt(20) + 1, namesymbols.toArray)
      var y = x
      while (y == x) y = randomTape(Random.nextInt(20) + 1, namesymbols.toArray)
      assert(y != x)
      val v = toTape(unaryString(Random.nextInt(20) + 1))

      val left = List(Nil, Nil)
      val right = List(Blank :: x, Blank :: y ++ List(Blank) ++ v)
      val expleft = left
      val expright = right

      testMachine(machine, left, right, expleft, expright, Reject)
    }
  }

  def get(): Unit = {
    Random.setSeed(0)
    val next = newNext()
    val namesymbols = ('a' to 'z').toSet
    val valuesymbols = Set('1')
    val machine = EnvStack.get(namesymbols, valuesymbols, '#', next)
    
    { // good case
      val n = Random.nextInt(4) + 2
      val xs = (1 to n).map(_ => randomTape(Random.nextInt(20) + 1, namesymbols.toArray)).toList
      val vs = (1 to n).map(_ => toTape(unaryString(Random.nextInt(20) + 1))).toList
      val ps = xs.zip(vs).map((x, v) => Blank :: x ++ List(Blank) ++ v)

      val i = Random.nextInt(n)

      val env = Alph('#') :: ps.flatten
      
      val left = List(Nil, env.reverse)
      val right = List(Blank :: xs(i), Nil)
      val expleft = List(Nil, env.reverse)
      val expright = List(Blank :: vs(i), Nil)
      
      testMachine(machine, left, right, expleft, expright, Accept)
    }
  }
}
