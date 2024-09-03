package turing

object ControlFlow {

  import TMUtil.*
  import TuringMachine.*

  def sequence[Q, A](m1: TuringMachine[Q, A], m2: TuringMachine[Q, A]): TuringMachine[Q, A] = {
    require(combinable(m1, m2))
    TuringMachine(m1.init, m1.transitions.map {
      case (q, s1) -> (Accept, s2, dir) =>
        (q, s1) -> (m2.init, s2, dir)
      case t => t
    } ++ m2.transitions)
  }

  def sequence[Q, A](m1: MultiMachine[Q, A], m2: MultiMachine[Q, A]): MultiMachine[Q, A] = {
    require(combinable(m1, m2))

    val (mm1, mm2) = ensureTapesMatch(m1, m2)

    MultiMachine(mm1.init, mm1.transitions.map {
      case (q, ss1) -> (Accept, ss2, dirs) =>
        (q, ss1) -> (mm2.init, ss2, dirs)
      case t => t
    } ++ mm2.transitions)
  }

  def sequence[Q, A](ms: List[TuringMachine[Q, A]]): TuringMachine[Q, A] =
    ms.reduce((a, b) => sequence(a, b))

  def sequence[Q, A](ms: List[MultiMachine[Q, A]]): MultiMachine[Q, A] =
    ms.reduce((a, b) => sequence(a, b))

  def ifThenElse[Q, A](cond: TuringMachine[Q, A], thn: TuringMachine[Q, A], els: TuringMachine[Q, A]): TuringMachine[Q, A] = {
    require(combinable(cond, thn) && combinable(cond, els) && combinable(thn, els))

    TuringMachine(cond.init, cond.transitions.map {
      case (q, s1) -> (Accept, s2, dir) => (q, s1) -> (thn.init, s2, dir)
      case (q, s1) -> (Reject, s2, dir) => (q, s1) -> (els.init, s2, dir)
      case t => t
    } ++ thn.transitions ++ els.transitions)
  }


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

  // insert the TM `child` into `parent`, such that 
  //  - each transition `q` -> `toChild` in `parent` becomes `q` -> `child.init`
  //  - each transition `q` -> `ha` in `child` becomes `q` -> `childAccept`
  //  - each transition `q` -> `hr` in `child` becomes `q` -> `childReject`
  def insertMachine[Q, A](parent: TuringMachine[Q, A], child: TuringMachine[Q, A], 
                          toChild: State[Q], childAccept: State[Q], childReject: State[Q]): TuringMachine[Q, A] = {
    require(combinable(parent, child))

    TuringMachine(parent.init, parent.transitions.map {
      case (q1, s1) -> (q2, s2, dir) if q2 == toChild =>
        (q1, s1) -> (child.init, s2, dir)
      case t => t
    } ++ child.transitions.map {
      case (q, s1) -> (Accept, s2, dir) =>
        (q, s1) -> (childAccept, s2, dir)
      case (q, s1) -> (Reject, s2, dir) =>
        (q, s1) -> (childReject, s2, dir)
      case t => t
    })
  }

  // insert the TM `child` into `parent`, such that 
  //  - each transition `q` -> `toChild` in `parent` becomes `q` -> `child.init`
  //  - each transition `q` -> `ha` in `child` becomes `q` -> `childAccept`
  //  - each transition `q` -> `hr` in `child` becomes `q` -> `childReject`
  def insertMachine[Q, A](parent: MultiMachine[Q, A], child: MultiMachine[Q, A], 
                          toChild: State[Q], childAccept: State[Q], childReject: State[Q]): MultiMachine[Q, A] = {
    require(combinable(parent, child))

    val (par, chi) = ensureTapesMatch(parent, child)
    MultiMachine(par.init, par.transitions.map {
      case (q1, ss1) -> (q2, ss2, dir) if q2 == toChild =>
        (q1, ss1) -> (chi.init, ss2, dir)
      case t => t
    } ++ chi.transitions.map {
      case (q1, ss1) -> (Accept, ss2, dir) =>
        (q1, ss1) -> (childAccept, ss2, dir)
      case (q1, ss1) -> (Reject, ss2, dir) => 
        (q1, ss1) -> (childReject, ss2, dir)
      case t => t
    })
  }

  case class MachineConnection[Q](to: State[Q], onAccept: State[Q], onReject: State[Q])
  def insertMachines[Q, A](parent: TuringMachine[Q, A], connections: List[(TuringMachine[Q, A], MachineConnection[Q])]): TuringMachine[Q, A] =
    connections.foldLeft(parent) {
      case (par, (child, conn)) => insertMachine(par, child, conn.to, conn.onAccept, conn.onReject)
    }

  def insertMachines[Q, A](parent: MultiMachine[Q, A], connections: List[(MultiMachine[Q, A], MachineConnection[Q])]): MultiMachine[Q, A] =
    connections.foldLeft(parent) {
      case (par, (child, conn)) => insertMachine(par, child, conn.to, conn.onAccept, conn.onReject)
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

  def prevBlank[A](symbols: Set[A], next: () => Int): TuringMachine[Int, A] = 
    TMManip.mirror(nextBlank(symbols, next))
}
