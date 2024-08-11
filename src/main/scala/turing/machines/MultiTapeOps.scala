package turing.machines

object MultiTapeOps {
  import turing.TuringMachine._
  import turing.TMUtil._

  def multiEq[A](symbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2) = initStates(3, next)

    val t1: MultiTable[Int, A] = Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right)),
      (q1, List(Blank, Blank)) -> (q2, List(Blank, Blank), List(Left, Left)),
      (q2, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
    )

    val t2 = symbols.foldLeft(t1)((tt, s1) => {
      val a = Alph(s1)
      tt +
        ((q1, List(a, a)) -> (q1, List(a, a), List(Right, Right))) +
        ((q2, List(a, a)) -> (q2, List(a, a), List(Left, Left)))
    })

    MultiMachine(q0, t2)
  }

  def copyDown[A](symbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2) = initStates(3, next)
    val table: MultiTable[Int, A] = Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right)),
      (q1, List(Blank, Blank)) -> (q2, List(Blank, Blank), List(Left, Left)),
      (q2, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
    )

    MultiMachine(q0, symbols.foldLeft(table)((tt, symb) => {
      val s = Alph(symb)
      tt +
        ((q1, List(s, Blank)) -> (q1, List(s, s), List(Right, Right))) +
        ((q2, List(s, s)) -> (q2, List(s, s), List(Left, Left)))
    }))
  }
}
