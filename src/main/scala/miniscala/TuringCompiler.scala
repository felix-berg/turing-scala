package miniscala

object TuringCompiler {
  
  import turing.TuringMachine._
  import turing.machines._
  import turing.{ TMManip, TMUtil, ControlFlow }
  import Ast._

  val HASH: Char = '#'
  val SPECIAL: Char = 'A'
  val COMMA: Char = ','
  val SEMICOLON: Char = ';'

  val ONE: Char = '1'
  val FALSE: Char = '?'
  val TRUE: Char = '^'

  val OBFUSCATEDCOMMA: Char = '+'
  val OBFUSCATEDSEMICOLON: Char = '-'
  val OBFUSCATEDCODEZERO: Char = '*'
  val OBFUSCATEDCODEONE: Char = '/'
  val OBFUSCATEDFUNCTIONSYMBOLS: Set[Char] = Set(OBFUSCATEDCODEONE, OBFUSCATEDCODEZERO, OBFUSCATEDSEMICOLON, OBFUSCATEDCOMMA) 
    ++ ('A' to 'Z') // note: A-Z are reserved for obscured variable names within function values

  val CODEZERO: Char = '&'
  val CODEONE: Char = '%'
  val CODELENGTH: Int = 5

  val VALIDNAMESYMBOLS: Set[Char] = ('a' to 'z').toSet
  val VALIDCODESYMBOLS: Set[Char] = Set(CODEONE, CODEZERO)
  val VALIDVALUESYMBOLS: Set[Char] = Set(ONE, FALSE, TRUE) ++ OBFUSCATEDFUNCTIONSYMBOLS

  assert(VALIDNAMESYMBOLS.intersect(VALIDVALUESYMBOLS).isEmpty)
  assert(VALIDNAMESYMBOLS.intersect(VALIDCODESYMBOLS).isEmpty)
  assert(VALIDVALUESYMBOLS.intersect(VALIDCODESYMBOLS).isEmpty)

  val MAINTAPE: Int = 0
  val ENVTAPE: Int = 1
  val CALLTAPE: Int = 2
  val NUMTAPES: Int = 3

  case class DeclaredFunctions(var map: Map[String, MultiMachine[Int, Char]], branchState: NonHalt[Int])

  private def onMainTape(m: TuringMachine[Int, Char]): MultiMachine[Int, Char] = 
    TMManip.workOn(m, MAINTAPE, NUMTAPES)

  private def binOpExp(leftexp: Exp, rightexp: Exp, op: BinOp, functions: DeclaredFunctions, next: () => Int): MultiMachine[Int, Char] = { 
    def composeMachine(lm: MultiMachine[Int, Char], rm: MultiMachine[Int, Char], op: TuringMachine[Int, Char], symbols: Set[Char]): MultiMachine[Int, Char] = {
      val nb = onMainTape(ControlFlow.nextBlank(symbols, next))
      val pb = onMainTape(ControlFlow.prevBlank(symbols, next))
      ControlFlow.sequence(List(lm, nb, rm, pb, onMainTape(op)))
    }

    val (lm, rm) = (compileImpl(leftexp, functions, next), compileImpl(rightexp, functions, next))
    op match {
      case PlusBinOp() => composeMachine(lm, rm, Unary.add(ONE, next), Set(ONE))
      case MinusBinOp() => composeMachine(lm, rm, Unary.sub(ONE, HASH, next), Set(ONE))
      case MultBinOp() => composeMachine(lm, rm, Unary.mul(ONE, HASH, SPECIAL, next), Set(ONE))
      case AndBinOp() => composeMachine(lm, rm, Bool.and(TRUE, FALSE, next), Set(TRUE, FALSE))
      case OrBinOp() => composeMachine(lm, rm, Bool.or(TRUE, FALSE, next), Set(TRUE, FALSE))
      case EqualBinOp() => 
        composeMachine(lm, rm, Bool.equality(Set(TRUE, FALSE, ONE), Set('t', 'f', 'o'), HASH, TRUE, FALSE, next), Set(TRUE, FALSE, ONE))
      case _ => ???
    }
  }
  

  private def addFunction(functions: DeclaredFunctions, bodyMachine: MultiMachine[Int, Char]): String = {
    val n = functions.map.size
    val bin = n.toBinaryString.map {
      case '0' => CODEZERO
      case '1' => CODEONE
      case _ => ???
    }.reverse.padTo(CODELENGTH, CODEZERO).reverse // pad to CODELENGTH

    functions.map = functions.map + (bin -> bodyMachine)
    bin
  }

  private def unobfuscateFunctionSymbol(c: Char): Char = {
    assert(OBFUSCATEDFUNCTIONSYMBOLS.contains(c))
    c match {
      case c if c >= 'A' && c <= 'Z' => (c - ('A' - 'a')).toChar
      case OBFUSCATEDCOMMA => COMMA
      case OBFUSCATEDSEMICOLON => SEMICOLON
      case OBFUSCATEDCODEONE => CODEONE
      case OBFUSCATEDCODEZERO => CODEZERO
      case _ => ???
    }
  }

  private def obfuscateFunctionSymbol(c: Char): Char = {
    c match {
      case c if c >= 'a' && c <= 'z' => (c + ('A' - 'a')).toChar
      case c if VALIDVALUESYMBOLS.contains(c) => c
      case COMMA => OBFUSCATEDCOMMA
      case SEMICOLON => OBFUSCATEDSEMICOLON
      case CODEONE => OBFUSCATEDCODEONE
      case CODEZERO => OBFUSCATEDCODEZERO
      case _ => ???
    }
  }

  def compileImpl(e: Exp, functions: DeclaredFunctions, next: () => Int): MultiMachine[Int, Char] = e match {
    case IntLit(c) => 
      onMainTape(SimpleOps.nSymbols(ONE, c, next))

    case BoolLit(b) => 
      onMainTape(SimpleOps.nSymbols(if (b) TRUE else FALSE, 1, next))

    case BinOpExp(leftexp, op, rightexp) => 
      binOpExp(leftexp, rightexp, op, functions, next)

    case IfThenElseExp(condexp, thenexp, elseexp) =>
      Bool.ifThenElse(compileImpl(condexp, functions, next), compileImpl(thenexp, functions, next), compileImpl(elseexp, functions, next), TRUE, FALSE, next)

    case BlockExp(decls, Nil, Nil, Nil, List(exp)) =>
      def evalDeclAndPush(decl: ValDecl): MultiMachine[Int, Char] = {
        // FIXME: doesn't handle not found name
        val writename = onMainTape(SimpleOps.write(decl.x.toList, next))
        val nb = onMainTape(ControlFlow.nextBlank(VALIDNAMESYMBOLS, next))
        val pb = onMainTape(ControlFlow.prevBlank(VALIDNAMESYMBOLS, next))
        val dm = compileImpl(decl.exp, functions, next)
        val push = TMManip.workOn(EnvStack.push(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, HASH, next), List(MAINTAPE, ENVTAPE), NUMTAPES)
        ControlFlow.sequence(List(writename, nb, dm, pb, push))
      }
      val em = compileImpl(exp, functions, next)
      val pops = decls.map(_ => TMManip.workOn(EnvStack.pop(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, next), List(MAINTAPE, ENVTAPE), NUMTAPES))
      ControlFlow.sequence(decls.map(evalDeclAndPush) ++ List(em) ++ pops)

    case VarExp(x) =>
      val writename = onMainTape(SimpleOps.write(x.toList, next))
      val get = TMManip.workOn(EnvStack.get(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, HASH, next), List(MAINTAPE, ENVTAPE), NUMTAPES)
      ControlFlow.sequence(List(writename, get))

    case UnOpExp(NotUnOp(), exp) =>
      ControlFlow.sequence(compileImpl(exp, functions, next), onMainTape(Bool.not(TRUE, FALSE, next)))

    case LambdaExp(params, body) => 
      assert(params.forall(p => p.x.toLowerCase == p.x))
      val bodyMachine = {
        val bodyWork = compileImpl(body, functions, next)
        val callReturnCode = onMainTape(SimpleOps.transitionToState(functions.branchState, next))
        ControlFlow.sequence(bodyWork, callReturnCode)
      }

      val code = addFunction(functions, bodyMachine)

      val putFunctionValue = TMManip.workOn(
        Functions.functionValue(
          params.map(p => p.x.toList), 
          code.toList, 
          VALIDNAMESYMBOLS ++ VALIDVALUESYMBOLS ++ VALIDCODESYMBOLS, 
          HASH, COMMA, SEMICOLON, next
        ), List(MAINTAPE, ENVTAPE), NUMTAPES)
      val obfuscateFunctionValue = onMainTape(
        SimpleOps.obfuscate(
          VALIDNAMESYMBOLS ++ VALIDVALUESYMBOLS ++ VALIDCODESYMBOLS +
            COMMA + SEMICOLON, obfuscateFunctionSymbol, next)
      )

      ControlFlow.sequence(putFunctionValue, obfuscateFunctionValue)
    
    case CallExp(funexp, args) =>
      val allsymbs = VALIDNAMESYMBOLS ++ VALIDVALUESYMBOLS ++ VALIDCODESYMBOLS + SEMICOLON + COMMA
      
      val putFunctionValue = ControlFlow.sequence(List(
        compileImpl(funexp, functions, next), 
        onMainTape(SimpleOps.obfuscate(OBFUSCATEDFUNCTIONSYMBOLS, unobfuscateFunctionSymbol, next)),
        onMainTape(SimpleOps.nextBlank(allsymbs, next))
      ))

      val machines = args.map(arg => compileImpl(arg, functions, next))
      val putArgs = machines.map(m => List(m, onMainTape(SimpleOps.nextBlank(allsymbs, next)))).flatten.dropRight(if (machines.nonEmpty) 1 else 0) // remove last nextblank
      val goBack = (1 to machines.size).map(_ => onMainTape(SimpleOps.prevBlank(allsymbs, next)))
      
      val preparefunccall = Functions.prepareFunctionCall(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, VALIDCODESYMBOLS, COMMA, SEMICOLON, HASH, next)
      val call = onMainTape(SimpleOps.transitionToState(functions.branchState, next))
      val afterReturned = Functions.cleanAfterFunctionCall(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, VALIDCODESYMBOLS, HASH, next)

      val returnCode = addFunction(functions, afterReturned)
      val putReturnCode = TMManip.workOn(ControlFlow.sequence(
        SimpleOps.write(returnCode.toList, next),
        SimpleOps.nextBlank(VALIDCODESYMBOLS, next)
      ), CALLTAPE, NUMTAPES)

      ControlFlow.sequence(
        putFunctionValue :: putArgs ++ goBack ++ List(putReturnCode, preparefunccall, call)
      )

    case _ => ???
  }

  private def addFunctionBrancher(m: MultiMachine[Int, Char], functions: DeclaredFunctions, next: () => Int): MultiMachine[Int, Char] = {
    val branches = functions.map.map {
      case (code, machine) => Functions.Branch(code.toList, machine.init)
    }.toList

    val brancher = ControlFlow.insertMachines(
      TMManip.workOn(Functions.brancher(branches, next), CALLTAPE, NUMTAPES), 
      functions.map.map {
        case (code, machine) => 
          machine -> ControlFlow.MachineConnection(machine.init, Accept, Reject)
      }.toList
    )

    val result = ControlFlow.insertMachine(m, brancher, functions.branchState, Accept, Reject)
    MultiMachine(result.init, result.transitions.map{
      case (q1, ss1) -> (functions.branchState, ss2, ds) => 
        (q1, ss1) -> (brancher.init, ss2, ds)
      case t => t
    })
  }

  def compile(e: Exp, next: () => Int): MultiMachine[Int, Char] = {
    val functions = DeclaredFunctions(Map(), NonHalt(next()))
    val machine = compileImpl(e, functions, next)
    if (functions.map.isEmpty) machine else
    addFunctionBrancher(machine, functions, next)
  }
}
