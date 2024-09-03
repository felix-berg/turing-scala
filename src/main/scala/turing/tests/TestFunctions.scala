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
      val env = Alph('#') :: ns.zip(vs).map{ case (s, v) => List(Blank) ++ s ++ List(Blank) ++ v }.flatten

      val thing = Blank :: env.map {
        case Blank => Alph(',')
        case Alph('#') => Blank
        case c => c
      }.drop(2)
      
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

  private def randomTapes(n: Int, arr: Array[Char] = ('a' to 'z').toArray): List[List[TapeAlph[Char]]] = 
    randomStrings(n, arr).map(s => s.map(c => Alph(c)))

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

  private def functionValue(): Unit = {
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
        "00",
        "1001", 
        "0011",
        "1011",
        "1010111"        
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

  def loadParamsIterate(): Unit = {
    val namesymbols = ('a' to 'z').toSet
    val valuesymbols = Set('1', 'T', 'F')
    val machine = Functions.Impl.loadParamsIterate(namesymbols, valuesymbols, ',', '#', newNext())
    { // last iteration
      val n = Random.between(1, 100)
      val right = List(
        Alph('#') :: (1 to n).map(_ => Blank).toList
        , Nil)
      val left = List(Nil, Nil)
      val expleft = List(Nil, Nil)
      val expright = List(Nil, Nil)

      testMachine(machine, 
        MultiConfig(left, machine.init, right),
        MultiConfig(expleft, Reject, expright)
      )
    }
    { // empty params case
      val left = List(Nil, Nil)
      val right = List(
        List(Alph('#')), Nil
      )
      val expright = List(Nil, Nil)
      val expleft = List(Nil, Nil)
      testMachine(machine, 
        MultiConfig(left, machine.init, right),
        MultiConfig(expleft, Reject, expright))
    }
    { // middle iteration
      val m = Random.between(3, 7)
      
      val ps = randomTapes(m, namesymbols.toArray)
      val as = randomTapes(m, valuesymbols.toArray)

      val psbefore = ps.map(s => Alph(',') :: s).flatten.tail
      val psafter = ps.drop(1).map(s => Alph(',') :: s).flatten.tail

      val pi = ps.head
      val ai = as.head

      val asbefore = as.map(s => Blank :: s).flatten.tail
      val asafter = as.drop(1).map(s => Blank :: s).flatten.tail

      val n = Random.between(1, 10)
      val middle = (1 to n).map(_ => Blank).toList

      val right = List(
        Alph('#') :: psbefore ++ middle ++ List(Blank) ++ asbefore,
        Nil
      )

      val blankpi = pi.map(_ => Blank)
      val blankai = ai.map(_ => Blank)

      val expleft = List(
        (Blank :: blankpi).reverse,
        (Blank :: pi ++ List(Blank) ++ ai).reverse
      )


      val expright = List(
        List(Alph('#')) ++  psafter ++ middle ++ List(Blank) ++ blankai ++ List(Blank) ++ asafter,
        Nil
      )

      testMachine(machine, 
        MultiConfig(List(Nil, Nil), machine.init, right),
        MultiConfig(expleft, Accept, expright)
      )
    }
  }

  def loadParamsToEnv(): Unit = {
    val namesymbols = ('a' to 'z').toSet
    val valuesymbols = Set('1', 'T', 'F')
    val machine = Functions.Impl.loadParamsToEnv(namesymbols, valuesymbols, ',', '#', newNext())
    { // |ps| = 0
      testMachine(machine, 
        MultiConfig(List(Nil, Nil), machine.init, List(Nil, Nil)),
        MultiConfig(List(Nil, Nil), Accept, List(Nil, Nil))
      )
    } 
    { // |ps| = 1
      val p = randomTape(Random.between(1, 8), namesymbols.toArray)
      val a = randomTape(Random.between(1, 8), valuesymbols.toArray)
      val right = List(Blank :: p ++ List(Blank) ++ a, Nil)
      val expright = List(Blank, Nil)
      val expleft = List(
        (Blank :: p.map(_ => Blank)).reverse, 
        (Blank :: p ++ List(Blank) ++ a).reverse
      )

      testMachine(machine,
        MultiConfig(List(Nil, Nil), machine.init, right),
        MultiConfig(expleft, Accept, List(Nil, Nil)))
    }
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
      val expleft = List(
        (Blank :: (1 to pscommas.size).map(_ => Blank).toList).reverse,
        result.reverse
      )
      
      testMachine(machine, 
        MultiConfig(left, machine.init, right),
        MultiConfig(expleft, Accept, expright))
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

  def prepareFunctionCall(): Unit = {
    def testCase(n: Int, m: Int, k: Int): Unit = {
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

      val venv = makeEnvironment(xs, vs)
      val penv = if (k == 0) Nil else Blank :: makeEnvironment(ps, as).drop(1) // remove #
      val newenv = env ++ List(Blank) ++ venv ++ penv
      val expleft = List(Nil, newenv.reverse, (Blank :: code.map(c => Alph(c))).reverse)
      val expright = List(Nil, Nil, Nil)

      testMachine(machine,
        MultiConfig(left, machine.init, right),
        MultiConfig(expleft, Accept, expright)
      )
    }

    {
      val n = Random.between(1, 4)
      val m = Random.between(1, 4)
      val k = Random.between(1, 4)
      testCase(n, m, k)
    }
    { 
      val m = Random.between(1, 4)
      val k = Random.between(1, 4)
      testCase(0, m, k)
    }
    { 
      val n = Random.between(1, 4)
      val k = Random.between(1, 4)
      testCase(n, 0, k)
    }
    { 
      val n = Random.between(1, 4)
      val m = Random.between(1, 4)
      testCase(n, m, 0)
    }
    {
      testCase(0, 0, 0)
    }
  }
}
