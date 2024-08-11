package turing.machines

import turing.tests.TestEnvStack.getValueIfMatches

object EnvStack {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.TMManip._
  import turing.ControlFlow

  /**
   * input:  [ΔxΔyΔ...](where x ∈ `namesymbols`, y ∈ `valuesymbols`)
   *          ^
   *         [Δ]
   *          ^
   * output: [ΔΔ^mΔΔ^nΔ...] (where m = |x|, n = |y|)
   *          ^
   *         [ΔxΔyΔ]
   *              ^
   */
  def push[A](namesymbols: Set[A], valuesymbols: Set[A], hash: A, next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2, q3) = initStates(4, next)

    val hs = Alph(hash)

    MultiMachine(q0, 
      Map(
        (q0, List(Blank, Blank)) -> (q1, List(hs, Blank), List(Right, Right)),
        (q1, List(Blank, Blank)) -> (q2, List(Blank, Blank), List(Right, Right)),
        (q2, List(Blank, Blank)) -> (q3, List(Blank, Blank), List(Left, Stay)),
        (q3, List(Blank, Blank)) -> (q3, List(Blank, Blank), List(Left, Stay)),
        (q3, List(hs, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay))
      ) 
      ++ namesymbols.map(s =>  (q1, List(Alph(s), Blank)) -> (q1, List(Blank, Alph(s)), List(Right, Right)))
      ++ valuesymbols.map(s => (q2, List(Alph(s), Blank)) -> (q2, List(Blank, Alph(s)), List(Right, Right)))
    )
  }

  /**
   * input:  [Δ]
   *          ^
   *         [ΔxΔyΔ] (where x ∈ `namesymbols`, y ∈ `valuesymbols`)
   *              ^
   * output: [Δ]
   *          ^
   *         [ΔΔ^mΔΔ^nΔ] (where m = |x|, n = |y|)
   *          ^
   */
  def pop[A](namesymbols: Set[A], valuesymbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2, q3) = initStates(4, next)

    MultiMachine(q0, 
      Map(
        (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Stay, Left)),
        (q1, List(Blank, Blank)) -> (q2, List(Blank, Blank), List(Stay, Left)),
        (q2, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay))
      ) 
      ++ namesymbols.map(s =>  (q2, List(Blank, Alph(s))) -> (q2, List(Blank, Blank), List(Stay, Left)))
      ++ valuesymbols.map(s => (q1, List(Blank, Alph(s))) -> (q1, List(Blank, Blank), List(Stay, Left)))
    )
  }

  object Impl {
    /**
     * TODO
     */
    def getValueIfMatches[A](namesymbols: Set[A], valuesymbols: Set[A], hash: A, next: () => Int): MultiMachine[Int, A] = {
      val List(q0, qequal, qnb, qerase, qcpy, qpb) = initStates(6, next)

      val equal = MultiTapeOps.equal(namesymbols, next)
      val nb = workOn(SimpleOps.nextBlank(namesymbols, next), 1, 2)
      val erase = workOn(SimpleOps.erase(namesymbols, hash, next), 0, 2)
      val cpy = workOn(MultiTapeOps.copyDown(valuesymbols, next), List(1, 0), 2)
      val pb = workOn(SimpleOps.prevBlank(namesymbols, next), 1, 2)

      val m1 = MultiMachine[Int, A](q0, Map(
        (q0, List(Blank, Blank)) -> (qequal, List(Blank, Blank), List(Stay, Stay))
      ))
      val m2 = ControlFlow.insertMachine(m1, equal, qequal, qnb, Reject)
      val m3 = ControlFlow.insertMachine(m2, nb, qnb, qerase, Reject)
      val m4 = ControlFlow.insertMachine(m3, erase, qerase, qcpy, Reject)
      val m5 = ControlFlow.insertMachine(m4, cpy, qcpy, qpb, Reject)
      val m6 = ControlFlow.insertMachine(m5, pb, qpb, Accept, Reject)
      m6
    }
  }

  /**
   * TODO
   */
  def get[A](namesymbols: Set[A], valuesymbols: Set[A], hash: A, next: () => Int): MultiMachine[Int, A] = {
    val List(q0, q1, q2, q3, q4, q5, qpb1, qpb2, qgetvalue, qfindhash1, qfindhash2) = initStates(11, next)
    
    val pb1 = workOn(SimpleOps.prevBlank(valuesymbols, next), 1, 2)
    val pb2 = workOn(SimpleOps.prevBlank(namesymbols, next), 1, 2)
    val getvalue = Impl.getValueIfMatches(namesymbols, valuesymbols, hash, next)
    val findhash1 = workOn(Unary.MulImpl.find(valuesymbols ++ namesymbols, hash, next), 1, 2)
    val findhash2 = workOn(Unary.MulImpl.find(valuesymbols ++ namesymbols, hash, next), 1, 2)

    val hs = Alph(hash)

    val m1 = MultiMachine(q0, Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Stay, Right)),
      (q1, List(Blank, Blank)) -> (q2, List(Blank, hs), List(Stay, Left)),
      (q2, List(Blank, Blank)) -> (q3, List(Blank, Blank), List(Stay, Left)),
      (q3, List(Blank, hs)) -> (qfindhash2, List(Blank, hs), List(Stay, Right)),
      (q4, List(Blank, hs)) -> (q4, List(Blank, Blank), List(Stay, Left)),
      (q4, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
      (q5, List(Blank, hs)) -> (q5, List(Blank, Blank), List(Stay, Left)),
      (q5, List(Blank, Blank)) -> (Reject, List(Blank, Blank), List(Stay, Stay))
    ) ++ valuesymbols.map(s => (q3, List(Blank, Alph(s))) -> (qpb1, List(Blank, Alph(s)), List(Stay, Right))))

    val m2 = ControlFlow.insertMachine(m1, pb1, qpb1, qpb2, Reject)
    val m3 = ControlFlow.insertMachine(m2, pb2, qpb2, qgetvalue, Reject)
    val m4 = ControlFlow.insertMachine(m3, getvalue, qgetvalue, qfindhash1, q2)
    val m5 = ControlFlow.insertMachine(m4, findhash1, qfindhash1, q4, Reject)
    val m6 = ControlFlow.insertMachine(m5, findhash2, qfindhash2, q5, Reject)

    m6
  }
} 
