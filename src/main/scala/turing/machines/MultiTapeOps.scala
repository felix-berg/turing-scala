package turing.machines

object MultiTapeOps {
  import turing.TuringMachine._
  import turing.TMUtil._

  def equal[A](symbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2, qr) = initStates(4, next)

    val t1: MultiTable[Int, A] = Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right)),
      (q1, List(Blank, Blank)) -> (q2, List(Blank, Blank), List(Left, Left)),
      (q2, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
      (qr, List(Blank, Blank)) -> (Reject, List(Blank, Blank), List(Stay, Stay))
    )

    val t2 = symbols.foldLeft(t1)((tt, s1) => {
      val a = Alph(s1)
      tt +
        ((q1, List(a, a)) -> (q1, List(a, a), List(Right, Right))) +
        ((q2, List(a, a)) -> (q2, List(a, a), List(Left, Left))) + 
        ((qr, List(a, a)) -> (qr, List(a, a), List(Left, Left)))
    })

    val cross = for { x <- symbols; y <- symbols } yield (x, y);
    val t3 = cross.foldLeft(t2) {
      case (tt, (s1, s2)) => 
        val a = Alph(s1)
        val b = Alph(s2)
        if (s1 == s2) tt else
        tt + ((q1, List(a, b)) -> (qr, List(a, b), List(Left, Left)))
    }

    MultiMachine(q0, t3)
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
  
  /**
   * input:  [Δx]
   *          ^
   *         [Δ]
   *          ^
   * output: [Δ]
   *          ^
   *         [Δx]
   *          ^
   */
  def moveDown[A](symbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2) = initStates(3, next)
    val table: MultiTable[Int, A] = Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right)),
      (q1, List(Blank, Blank)) -> (q2, List(Blank, Blank), List(Left, Left)),
      (q2, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
    )

    MultiMachine(q0, symbols.foldLeft(table)((tt, symb) => {
      val s = Alph(symb)
      tt +
        ((q1, List(s, Blank)) -> (q1, List(Blank, s), List(Right, Right))) +
        ((q2, List(Blank, s)) -> (q2, List(Blank, s), List(Left, Left)))
    }))
  }

  /**
   * input:  [Δx]
   *          ^
   *         [Δ]
   *          ^
   * output: [ΔΔ^nΔ] (where n = |x|)
   *              ^
   *         [Δx]
   *          ^
   */
  def moveDown2[A](symbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1) = initStates(2, next)
    MultiMachine(q0, Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right)),
      (q1, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay))
    ) ++ symbols.map(s => List(
      (q1, List(Alph(s), Blank)) -> (q1, List(Blank, Alph(s)), List(Right, Right))
    )).flatten)
  }

  /**
   * input:  [Δx1Δx2Δ...Δxn;...]
   *          ^
   *         [Δ]
   *          ^
   * output: [ΔΔΔΔΔΔΔΔΔΔΔΔΔΔ...]
   *                       ^
   *         [Δx1Δx2Δ...ΔxnΔ]
   *                       ^
   */
  def moveDownUntil[A](symbols: Set[A], stop: A, next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1) = initStates(2, next)
    MultiMachine(q0, Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right)),
      (q1, List(Alph(stop), Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
      (q1, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Right, Right))
    ) ++ symbols.map(s => 
      (q1, List(Alph(s), Blank)) -> (q1, List(Blank, Alph(s)), List(Right, Right))
    ))
  }
}
