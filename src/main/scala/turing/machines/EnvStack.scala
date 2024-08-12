package turing.machines

import turing.tests.TestEnvStack.getValueIfMatches

object EnvStack {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.TMManip._
  import turing.ControlFlow._

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

      val par = MultiMachine[Int, A](q0, Map(
        (q0, List(Blank, Blank)) -> (qequal, List(Blank, Blank), List(Stay, Stay))
      ))
      insertMachines(par, List(
        (equal, MachineConnection(qequal, qnb, Reject)),
        (nb, MachineConnection(qnb, qerase, Reject)),
        (erase, MachineConnection(qerase, qcpy, Reject)),
        (cpy, MachineConnection(qcpy, qpb, Reject)),
        (pb, MachineConnection(qpb, Accept, Reject))
      ))
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

    val par = MultiMachine(q0, Map(
      (q0, List(Blank, Blank)) -> (q1, List(Blank, Blank), List(Stay, Right)),
      (q1, List(Blank, Blank)) -> (q2, List(Blank, hs), List(Stay, Left)),
      (q2, List(Blank, Blank)) -> (q3, List(Blank, Blank), List(Stay, Left)),
      (q3, List(Blank, hs)) -> (qfindhash2, List(Blank, hs), List(Stay, Right)),
      (q4, List(Blank, hs)) -> (q4, List(Blank, Blank), List(Stay, Left)),
      (q4, List(Blank, Blank)) -> (Accept, List(Blank, Blank), List(Stay, Stay)),
      (q5, List(Blank, hs)) -> (q5, List(Blank, Blank), List(Stay, Left)),
      (q5, List(Blank, Blank)) -> (Reject, List(Blank, Blank), List(Stay, Stay))
    ) ++ valuesymbols.map(s => (q3, List(Blank, Alph(s))) -> (qpb1, List(Blank, Alph(s)), List(Stay, Right))))

    insertMachines(par, List(
      (pb1, MachineConnection(qpb1, qpb2, Reject)),
      (pb2, MachineConnection(qpb2, qgetvalue, Reject)),
      (getvalue, MachineConnection(qgetvalue, qfindhash1, q2)),
      (findhash1, MachineConnection(qfindhash1, q4, Reject)),
      (findhash2, MachineConnection(qfindhash2, q5, Reject))
    ))
  }
} 
