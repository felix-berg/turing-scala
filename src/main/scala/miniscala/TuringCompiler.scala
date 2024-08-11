package miniscala

object TuringCompiler {
  
  import turing.TuringMachine._
  import turing.machines._
  import turing.ControlFlow
  import Ast._

  val FALSE: Char = 'F'
  val TRUE: Char = 'T'

  private def binOpExp(leftexp: Exp, rightexp: Exp, op: BinOp, next: () => Int): TuringMachine[Int, Char] = { 
    def composeMachine(lm: TuringMachine[Int, Char], rm: TuringMachine[Int, Char], op: TuringMachine[Int, Char], symbols: Set[Char]): TuringMachine[Int, Char] = {
      val nb = ControlFlow.nextBlank(symbols, next)
      val pb = ControlFlow.prevBlank(symbols, next)
      ControlFlow.sequence(List(lm, nb, rm, pb, op))
    }

    val (lm, rm) = (compile(leftexp, next), compile(rightexp, next))
    op match {
      case PlusBinOp() => composeMachine(lm, rm, Unary.add('1', next), Set('1'))
      case MinusBinOp() => composeMachine(lm, rm, Unary.sub('1', '#', next), Set('1'))
      case MultBinOp() => composeMachine(lm, rm, Unary.mul('1', '#', 'A', next), Set('1'))
      case AndBinOp() => composeMachine(lm, rm, Bool.and(TRUE, FALSE, next), Set(TRUE, FALSE))
      case OrBinOp() => composeMachine(lm, rm, Bool.or(TRUE, FALSE, next), Set(TRUE, FALSE))
      case _ => ???
    }
  }

  def compile(e: Exp, next: () => Int): TuringMachine[Int, Char] = {
    e match {
      case IntLit(c) => SimpleOps.nSymbols('1', c, next)
      case BoolLit(b) => SimpleOps.nSymbols(if (b) TRUE else FALSE, 1, next)
      case BinOpExp(leftexp, op, rightexp) => binOpExp(leftexp, rightexp, op, next)
      case IfThenElseExp(condexp, thenexp, elseexp) =>
        Bool.ifThenElse(compile(condexp, next), compile(thenexp, next), compile(elseexp, next), TRUE, FALSE, next)
      case _ => ???
    }
  }
  
}
