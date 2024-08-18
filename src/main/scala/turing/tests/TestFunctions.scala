package turing.tests

object TestFunctions {
  import turing.TuringMachine._
  import turing.TMSimulate._
  import turing.machines._
  import TestUtils._
  import scala.util.Random

  private def functionValue(): Unit = {
    val machine = Functions.functionValue(List("asad".toList, "asdb".toList), "ABABB".toList, ',', ';', newNext())
  }
  
  def writeParams(): Unit = {
    { // >= 1 params
      val n = Random.nextInt(20) + 1
      val params = (1 to n).map(_ => randomString(Random.nextInt(20) + 1, ('a' to 'z').toArray)).toList
      val machine = Functions.Impl.writeParams(params, '|', newNext())
      val expright = Blank :: params.map(p => p.map(c => Alph(c)) ++ List(Alph('|'))).flatten.dropRight(1)
      testMachine(machine, Nil, Nil, Nil, expright)
    }
    { // 0 params
      val machine = Functions.Impl.writeParams(Nil, '|', newNext())
      testMachine(machine, Nil, Nil, Nil, Nil)
    }
  } 
}
