package turing

object ControlFlow {

  import TMUtil.*
  import TuringMachine.*


  def sequence[Q, A](m1: MultiMachine[Q, A], m2: MultiMachine[Q, A]): MultiMachine[Q, A] = {
    require(combinable(m1, m2))

    val (mm1, mm2) = ensureTapesMatch(m1, m2)

    MultiMachine(mm1.init, mm1.transitions.map {
      case (q, ss1) -> (Accept, ss2, dirs) =>
        (q, ss1) -> (mm2.init, ss2, dirs)
      case t => t
    } ++ mm2.transitions)
  }

  def sequence[Q, A](ms: List[MultiMachine[Q, A]]): MultiMachine[Q, A] =
    ms.reduce((a, b) => sequence(a, b))

  def ifThenElse[Q, A](cond: MultiMachine[Q, A], thn: MultiMachine[Q, A], els: MultiMachine[Q, A]): MultiMachine[Q, A] = {
    require(combinable(cond, thn) && combinable(cond, els) && combinable(thn, els))

    val (cc, tt, ee) = ensureTapesMatch(cond, thn, els)
    MultiMachine(cc.init, cc.transitions.map {
      case (q, ss1) -> (Accept, ss2, dirs) => (q, ss1) -> (tt.init, ss2, dirs)
      case (q, ss1) -> (Reject, ss2, dirs) => (q, ss1) -> (ee.init, ss2, dirs)
      case t => t
    } ++ tt.transitions ++ ee.transitions)
  }

  def whileLoop[Q, A](cond: MultiMachine[Q, A], body: MultiMachine[Q, A]): MultiMachine[Q, A] = {
    require(combinable(cond, body))
    val (cc, bb) = ensureTapesMatch(cond, body)

    MultiMachine(cc.init, cc.transitions.map {
      case (q, ss1) -> (Accept, ss2, dirs) =>
        (q, ss1) -> (bb.init, ss2, dirs)
      case (q, ss1) -> (Reject, ss2, dirs) =>
        (q, ss1) -> (Accept, ss2, dirs)
      case t => t
    } ++ bb.transitions.map {
      case (q, ss1) -> (Accept, ss2, dirs) =>
        (q, ss1) -> (cc.init, ss2, dirs)
      case t => t
    })
  }

  def nextBlank[A](symbols: Set[A], next: () => Int): TuringMachine[Int, A] = {
    val (q0, q1) = (NonHalt(next()), NonHalt(next()))
    val table = symbols.map(s => Alph(s))
      .foldLeft[TransTable[Int, A]](Map())((tt, s) => tt +
        ((q0, s) -> (q1, s, Right)) +
        ((q1, s) -> (q1, s, Right))
      )
        + ((q0, Blank) -> (q1, Blank, Right))
        + ((q1, Blank) -> (Accept, Blank, Stay))

    TuringMachine(q0, table)
  }

  def prevBlank[Int, A](symbols: Set[A], next: () => Int): TuringMachine[Int, A] = {
    val (q0, q1) = (NonHalt(next()), NonHalt(next()))
    val table = symbols.map(s => Alph(s))
      .foldLeft[TransTable[Int, A]](Map())((tt, s) => tt +
        ((q0, s) -> (q1, s, Left)) +
        ((q1, s) -> (q1, s, Left))
      )
      + ((q0, Blank) -> (q1, Blank, Left))
      + ((q1, Blank) -> (Accept, Blank, Stay))

    TuringMachine(q0, table)
  }
}
