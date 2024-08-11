package turing.machines

object Bool {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.ControlFlow

  /**
   * input:  Δb... (where b ∈ { `t`, `f` })
   *         ^
   * output: 
   *       - Δb... (accept), if b = `t`
   *         ^
   *       - Δb... (reject), if b = `f`
   *         ^
   */
  def test[A](t: A, f: A, next: () => Int): TuringMachine[Int, A] = {
    val List(q0, q1) = initStates(2, next)
    val tr = Alph(t)
    val fl = Alph(f)

    TuringMachine(q0, Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, tr) -> (Accept, tr, Left),
      (q1, fl) -> (Reject, fl, Left)
    ))
  }

  /**
   * input:  Δb... (where b ∈ { `t`, `f` })
   *         ^
   * output: Δc...  (where c = NOT b)
   *         ^
   */
  def not[A](t: A, f: A, next: () => Int): TuringMachine[Int, A] = {
    val List(q0, q1) = initStates(2, next)
    val tr = Alph(t)
    val fl = Alph(f)

    TuringMachine(q0, Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, tr) -> (Accept, fl, Left),
      (q1, fl) -> (Accept, tr, Left)
    ))
  }

  /**
   * input:  ΔaΔb... (where a, b ∈ { `t`, `f` })
   *         ^
   * output: ΔcΔΔ... (where c = table(a, b))
   *         ^
   */
  def binOpFromTruthTable[A](t: A, f: A, table: (Boolean, Boolean) => Boolean, next: () => Int): TuringMachine[Int, A] = {
    val qs = initStates(8, next)
    val List(q0, q1, q2, q3, q4, q5, qt, qf) = qs
    def getState(a: Boolean, b: Boolean): State[Int] = 
      if (table(a, b)) qt else qf
    val tr = Alph(t)
    val fl = Alph(f)
    
    TuringMachine(q0, Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, tr) -> (q2, tr, Right),
      (q1, fl) -> (q3, fl, Right),
      (q2, Blank) -> (q4, Blank, Right),
      (q3, Blank) -> (q5, Blank, Right),
      (q4, tr) -> (getState(true, true), Blank, Left),
      (q4, fl) -> (getState(true, false), Blank, Left),
      (q5, tr) -> (getState(false, true), Blank, Left),
      (q5, fl) -> (getState(false, false), Blank, Left),
      (qf, Blank) -> (qf, Blank, Left),
      (qf, tr) -> (Accept, fl, Left),
      (qf, fl) -> (Accept, fl, Left),
      (qt, Blank) -> (qt, Blank, Left),
      (qt, tr) -> (Accept, tr, Left),
      (qt, fl) -> (Accept, tr, Left),
    ))
  }

  /**
   * input:  ΔaΔb... (where a, b ∈ { `t`, `f` })
   *         ^
   * output: ΔcΔΔ... (where c = a AND b)
   */
  def and[A](t: A, f: A, next: () => Int): TuringMachine[Int, A] = binOpFromTruthTable(t, f, (a, b) => a & b, next)

  /**
   * input:  ΔaΔb... (where a, b ∈ { `t`, `f` })
   *         ^
   * output: ΔcΔΔ... (where c = a OR b)
   */
  def or[A](t: A, f: A, next: () => Int): TuringMachine[Int, A] = binOpFromTruthTable(t, f, (a, b) => a | b, next)

  def ifThenElse[A](cond: TuringMachine[Int, A], thn: TuringMachine[Int, A], els: TuringMachine[Int, A], t: A, f: A, next: () => Int): TuringMachine[Int, A] = {
    val List(q0, q1, q2, qcond, qthn, qels) = initStates(6, next)
    val m1 = TuringMachine[Int, A](q0, Map(
      (q0, Blank) -> (qcond, Blank, Stay),
      (q1, Blank) -> (q2, Blank, Right),
      (q2, Alph(t)) -> (qthn, Blank, Left),
      (q2, Alph(f)) -> (qels, Blank, Left)
    ))
    val m2 = ControlFlow.insertMachine(m1, cond, qcond, q1, Reject)
    val m3 = ControlFlow.insertMachine(m2, thn, qthn, Accept, Reject)
    val m4 = ControlFlow.insertMachine(m3, els, qels, Accept, Reject)
    m4
  }
}