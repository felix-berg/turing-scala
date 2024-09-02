package turing.tests

object TestFunctions {
  import turing.TuringMachine._
  import turing.TMSimulate._
  import turing.machines._
  import TestUtils._
  import scala.util.Random

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

  def copyEnv(): Unit = {
    val machine = Functions.Impl.copyEnv(('a' to 'z').toSet + '1' + 'T' + 'F', ',', '#', newNext())
    { // > 0 pairs in environment
      val n = Random.between(1, 10)
      val ns = (1 to n).map(_ => randomTape(Random.between(1, 6), ('a' to 'z').toArray)).toList
      val vs = (1 to n).map(_ => randomTape(Random.between(1, 10), Array('1', 'T', 'F'))).toList
      val env = Alph('#') :: ns.zip(vs).map{ case (s, v) => s ++ List(Blank) ++ v ++ List(Blank) }.flatten.dropRight(1)

      val thing = env.map {
        case Blank => Alph(',')
        case Alph('#') => Blank
        case c => c
      }
      
      val left = List(Nil, env.reverse)
      val right = List(Nil, Nil)
      val expleft = List(thing.reverse, env.reverse)
      val expright = List(Nil, Nil)
      
      testMachine(machine, MultiConfig(left, machine.init, right), MultiConfig(expleft, Accept, expright))
    }
    { // 0 pairs in env.
      val left = List(Nil, List(Alph('#')))
      val right = List(Nil, Nil)
      val expleft = List(List(Blank), List(Alph('#')))
      val expright = List(Nil, Nil)

      testMachine(machine, MultiConfig(left, machine.init, right), MultiConfig(expleft, Accept, expright))
    }
  }

  private def randomStrings(n: Int, arr: Array[Char] = ('a' to 'z').toArray): List[List[Char]] = 
    (1 to n).map(_ => randomString(Random.between(1, 20), arr)).toList

  private def toTapeWithBlanks(xs: List[List[Char]]): List[TapeAlph[Char]] = 
    xs.map(x => Blank :: x.map(c => Alph(c))).flatten

  private def makeFunctionValue(params: List[List[Char]], names: List[List[Char]], values: List[List[Char]], code: List[Char]): List[TapeAlph[Char]] = {
    require(names.size == values.size)
    val n = names.size
    val xs = names.map(n => n.map(c => Alph(c)))
    val vs = values.map(v => v.map(c => Alph(c)))
    val psvalue = params.map(param => param.map(c => Alph(c)) ++ List(Alph(','))).flatten.dropRight(if (params.isEmpty) 0 else 1)
    val envvalue = xs.zip(vs).map((x, v) => x ++ List(Alph(',')) ++ v ++ List(Alph(','))).flatten.dropRight(if (n == 0) 0 else 1)
    Blank ::  code.map(c => Alph(c)) ++ List(Alph(';')) ++ envvalue ++ List(Alph(';')) ++ psvalue
  }

  private def makeEnvironment(names: List[List[Char]], values: List[List[Char]]): List[TapeAlph[Char]] = {
    require(names.size == values.size)
    val n = names.size
    val xs = names.map(n => n.map(c => Alph(c)))
    val vs = values.map(v => v.map(c => Alph(c)))
    Alph('#') :: xs.zip(vs).map((x, v) => x ++ List(Blank) ++ v ++ List(Blank)).flatten.dropRight(if (n == 0) 0 else 1)
  }

  def functionValue(): Unit = {
    def f(params: List[List[Char]], code: List[Char], envMin: Int, envMax: Int): Unit = {
      val machine = Functions.functionValue(params, code, ('a' to 'z').toSet ++ Set('1', 'T', 'F'), '#', ',', ';', newNext())

      val n = Random.between(envMin, envMax)
      val xs = randomStrings(n, ('a' to 'z').toArray)
      val vs = randomStrings(n, Array('1', 'T', 'F'))

      val value = makeFunctionValue(params, xs, vs, code)
      val env = makeEnvironment(xs, vs)

      val left = List(Nil, env.reverse)
      val right = List(Nil, Nil)
      val expleft = List(Nil, env.reverse)
      val expright = List(value, Nil)

      testMachine(machine, MultiConfig(left, machine.init, right), MultiConfig(expleft, Accept, expright))
    }

    val ps = (1 to Random.between(1, 10)).map(_ => randomString(Random.nextInt(20) + 1, ('a' to 'z').toArray)).toList
    val code = "ABBABBABAB".toList
    f(ps, code, 0, 10)
    f(ps, code, 0, 1)
    f(Nil, code, 0, 10)
    f(Nil, code, 0, 1)
  }
  
  def brancher(): Unit = {
    val next = newNext()
    def testBranches(branches: List[Functions.Branch[Char]]): Unit = {
      val machine = Functions.brancher(branches, next)
      for (branch <- branches) {
        val codetape = Blank :: branch.code.map(c => Alph(c)) 
        var conf = Configuration(codetape.reverse, machine.init, Nil)
        while (conf.state != branch.state && conf.state != Reject && conf.state != Accept) {
          conf = step(machine, conf)
        }
        assert(conf.state == branch.state, s"unexpected halt with $conf")
      }
    }
    
    { // some random equal length branches (no ambiguity)
      val n = Random.between(1, 20) 
      val codes = (1 to n).map(_ => randomString(7, ('a' to 'b').toArray)).toSet
      val branches = codes.map(c => Functions.Branch(c, NonHalt(next()))).toList
      testBranches(branches)
    }
    { // some not-equal-length branches
      val branches: List[Functions.Branch[Char]] = List(
        "01", 
        "00",
        "1001", 
        "1000",
        "1101",
        "1110101",
      ).map(s => Functions.Branch(s.toList, NonHalt(next())))
      testBranches(branches)
    }
    { // single letter codes
      val branches = List(
        "0", "1", "2", "3"
      ).map(s => Functions.Branch(s.toList, NonHalt(next())))
      testBranches(branches)
    }
  }

  def replaceBehindUntil(): Unit = {
    val garbage = randomStrings(10).map(s => Blank :: s.map(c => Alph(c))).flatten
    val symbols = ('a' to 'z').toSet
    val (stop, from) = ('#', ',')
    val words = (1 to Random.between(1, 10)).map(_ => randomTape(Random.between(1, 5), symbols.toArray))
    val machine = Functions.Impl.replaceWithBlankBehindUntil(symbols, stop, from, 'A', newNext())
    val left = garbage ++ List(Alph(stop)) ++ words.map(w => Blank :: w).flatten
    val expleft = garbage ++ List(Alph(stop)) ++ words.map(w => Blank :: w.map {
      case Alph(',') => Blank
      case t => t
    }).flatten
    testMachine(machine, left, Nil, expleft, Nil)
  }

  private def findOptions(): Unit = {
    val symbols = ('a' to 'z').toSet
    val machine = Functions.Impl.findOptions(symbols, List(Alph('A') -> Accept, Alph('R') -> Reject), newNext())
    val garbage = randomTape(Random.between(1, 100), ('a' to 'z').toArray)
    {
      val x = randomTape(Random.between(1, 10), symbols.toArray)
      val right = Alph('f') :: x ++ List(Alph('A')) ++ garbage
      val expleft = (Alph('f') :: x).reverse
      val expright = Alph('A') :: garbage
      testMachine(machine, Nil, right, expleft, expright, Accept)
    }
    {
      val x = randomTape(Random.between(1, 10), symbols.toArray)
      val right = Alph('f') :: x ++ List(Alph('R')) ++ garbage
      val expleft = (Alph('f') :: x).reverse
      val expright = Alph('R') :: garbage
      printRunConfiguration(machine, Configuration(Nil, machine.init, right), 100)
      testMachine(machine, Nil, right, expleft, expright, Reject)
    }
    // {
    //   val x = Nil
    //   val right = Alph('f') :: x ++ List(Alph('R')) ++ garbage
    //   val expleft = (Alph('f') :: x).reverse
    //   val expright = Alph('R') :: garbage
    //   testMachine(machine, Nil, right, expleft, expright, Reject)
    // }
    // {
    //   val x = Nil
    //   val right = Alph('f') :: x ++ List(Alph('A')) ++ garbage
    //   val expleft = (Alph('f') :: x).reverse
    //   val expright = Alph('A') :: garbage
    //   testMachine(machine, Nil, right, expleft, expright, Accept)
    // }
    // {
    //   val x = Nil
    //   val right = Alph('A') :: x ++ List(Alph('A')) ++ garbage
    //   val expleft = (Alph('A') :: x).reverse
    //   val expright = Alph('A') :: garbage
    //   testMachine(machine, Nil, right, expleft, expright, Accept)
    // }
    // {
    //   val x = Nil
    //   val right = Alph('R') :: x ++ List(Alph('A')) ++ garbage
    //   val expleft = (Alph('R') :: x).reverse
    //   val expright = Alph('A') :: garbage
    //   testMachine(machine, Nil, right, expleft, expright, Accept)
    // }
  }

  private def loadParamsToEnv(): Unit = {
    val namesymbols = ('a' to 'z').toSet
    val valuesymbols = Set('1', 'T', 'F')
    val machine = Functions.Impl.loadParamsToEnv(namesymbols, valuesymbols, ',', '#', newNext())
    // TODO: { // |ps| = 0
    //   
    // } 
    { // |ps| > 1
      val k = Random.between(1, 7)
      val ps = randomStrings(k, namesymbols.toArray)
      val as = randomStrings(k, valuesymbols.toArray)
      
      val pscommas = ps.map(param => Alph(',') :: param.map(c => Alph(c))).flatten.tail
      val args = as.map(arg => Blank :: arg.map(c => Alph(c))).flatten
      val result = ps.zip(as).map((param, arg) => Blank :: param.map(c => Alph(c)) ++ List(Blank) ++ arg.map(c => Alph(c))).flatten
      val right = List(Blank :: pscommas ++ args, Nil)
      val left = List(Nil, Nil)
      val expright = List(Nil, Nil)
      val expleft = List(Nil, result.reverse)

      testMachine(machine, 
        MultiConfig(left, machine.init, right),
        MultiConfig(expleft, Accept, expright))
    }
  }

  private def prepareFunctionCall(): Unit = {
    { // env >= 1, ps >=1
      val n = Random.between(1, 4)
      val m = Random.between(1, 4)
      val k = Random.between(1, 4)
      
      val code = randomString(Random.between(1, 5), Array('A', 'B'))
      val ps = randomStrings(k)
      val xs = randomStrings(n)
      val vs = randomStrings(n, Array('1', 'T', 'F'))
      val as = randomStrings(k, Array('1', 'T', 'F'))

      val ys = randomStrings(m)
      val us = randomStrings(m, Array('1', 'T', 'F'))

      val env = makeEnvironment(ys, us)
      val value = makeFunctionValue(ps, xs, vs, code)
      val arguments = as.map(arg => Blank :: arg.map(c => Alph(c))).flatten

      val namesymbols = ('a' to 'z').toSet
      val valuesymbols = Set('1', 'T', 'F')
      val codesymbols = Set('A', 'B')

      val machine = Functions.prepareFunctionCall(namesymbols, valuesymbols, codesymbols, ',', ';', '#', newNext())

      val left = List(Nil, env.reverse, Nil)
      val right = List(value ++ arguments, Nil, Nil)

      val yenv = Blank :: makeEnvironment(ys, us).drop(1) // remove #
      val penv = Blank :: makeEnvironment(ps, as).drop(1) 
      val newenv = env ++ yenv ++ penv
      val expleft = List(Nil, newenv.reverse, (Blank :: code).reverse)
      val expright = List(Nil, Nil, Nil)

      testMachine(machine,
        MultiConfig(left, machine.init, right),
        MultiConfig(Nil, Accept, Nil)
      )
    }
    // TODO: empty env, params
  }
}
