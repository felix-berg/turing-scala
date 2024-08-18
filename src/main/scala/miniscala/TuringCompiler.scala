package miniscala

object TuringCompiler {
  
  import turing.TuringMachine._
  import turing.machines._
  import turing.{ TMManip, TMUtil, ControlFlow }
  import Ast._

  val ONE: Char = '1'
  val FALSE: Char = 'F'
  val TRUE: Char = 'T'
  val HASH: Char = '#'
  val SPECIAL: Char = 'A'

  val VALIDNAMESYMBOLS: Set[Char] = ('1' to '9').toSet ++ ('a' to 'z').toSet ++ ('A' to 'Z').toSet + '_'
  val VALIDVALUESYMBOLS: Set[Char] = Set(ONE, FALSE, TRUE)

  private def onMainTape(m: TuringMachine[Int, Char]): MultiMachine[Int, Char] = 
    TMManip.workOn(m, 0, 2)

  private def binOpExp(leftexp: Exp, rightexp: Exp, op: BinOp, next: () => Int): MultiMachine[Int, Char] = { 
    def composeMachine(lm: MultiMachine[Int, Char], rm: MultiMachine[Int, Char], op: TuringMachine[Int, Char], symbols: Set[Char]): MultiMachine[Int, Char] = {
      val nb = onMainTape(ControlFlow.nextBlank(symbols, next))
      val pb = onMainTape(ControlFlow.prevBlank(symbols, next))
      ControlFlow.sequence(List(lm, nb, rm, pb, onMainTape(op)))
    }

    val (lm, rm) = (compile(leftexp, next), compile(rightexp, next))
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

  def compile(e: Exp, next: () => Int): MultiMachine[Int, Char] = {
    e match {
      case IntLit(c) => 
        onMainTape(SimpleOps.nSymbols(ONE, c, next))

      case BoolLit(b) => 
        onMainTape(SimpleOps.nSymbols(if (b) TRUE else FALSE, 1, next))

      case BinOpExp(leftexp, op, rightexp) => 
        binOpExp(leftexp, rightexp, op, next)

      case IfThenElseExp(condexp, thenexp, elseexp) =>
        Bool.ifThenElse(compile(condexp, next), compile(thenexp, next), compile(elseexp, next), TRUE, FALSE, next)

      case BlockExp(decls, Nil, Nil, Nil, List(exp)) =>
        def evalDeclAndPush(decl: ValDecl): MultiMachine[Int, Char] = {
          // FIXME: doesn't handle not found name
          val writename = onMainTape(SimpleOps.write(decl.x.toList, next))
          val nb = onMainTape(ControlFlow.nextBlank(VALIDNAMESYMBOLS, next))
          val pb = onMainTape(ControlFlow.prevBlank(VALIDNAMESYMBOLS, next))
          val dm = compile(decl.exp, next)
          val push = EnvStack.push(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, HASH, next)
          ControlFlow.sequence(List(writename, nb, dm, pb, push))
        }
        val em = compile(exp, next)
        val pops = decls.map(_ => EnvStack.pop(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, next))
        ControlFlow.sequence(decls.map(evalDeclAndPush) ++ List(em) ++ pops)

      case VarExp(x) =>
        val writename = onMainTape(SimpleOps.write(x.toList, next))
        val get = EnvStack.get(VALIDNAMESYMBOLS, VALIDVALUESYMBOLS, HASH, next)
        ControlFlow.sequence(List(writename, get))

      case UnOpExp(NotUnOp(), exp) =>
        ControlFlow.sequence(compile(exp, next), onMainTape(Bool.not(TRUE, FALSE, next)))

      case _ => ???
    }
  }
}
