package turing

import turing.TuringMachine.*

object TMUtil {

  def numTapes[Q, A](m: MultiMachine[Q, A]): Int = {
    require(m.transitions.nonEmpty)
    m.transitions.head._1._2.length
  }

  def valid[Q, A](m: MultiMachine[Q, A]): Boolean = {
    val n = numTapes(m)
    m.transitions.forall {
      case (q1, ss1) -> (q2, ss2, dirs) =>
        n == ss1.length &&
        n == ss2.length &&
        n == dirs.length
    }
  }

  def combinable[Q, A](m1: TuringMachine[Q, A], m2: TuringMachine[Q, A]): Boolean =
    m1.transitions.keySet.intersect(m2.transitions.keySet).isEmpty

  def combinable[Q, A](m1: MultiMachine[Q, A], m2: MultiMachine[Q, A]): Boolean =
    valid(m1) && valid(m2) &&
      numTapes(m1) == numTapes(m2) &&
      m1.transitions.keySet.intersect(m2.transitions.keySet).isEmpty

  def ensureTapesMatch[Q, A](m1: MultiMachine[Q, A], m2: MultiMachine[Q, A]): (MultiMachine[Q, A], MultiMachine[Q, A]) = {
    import TMManip.enlarge

    require(valid(m1) && valid(m2))
    val max = Math.max(numTapes(m1), numTapes(m2))
    val mm1 = if (numTapes(m1) == max) m1 else enlarge(m1, max)
    val mm2 = if (numTapes(m2) == max) m2 else enlarge(m2, max)
    (mm1, mm2)
  }

  def ensureTapesMatch[Q, A](m1: MultiMachine[Q, A], m2: MultiMachine[Q, A], m3: MultiMachine[Q, A]): (MultiMachine[Q, A], MultiMachine[Q, A], MultiMachine[Q, A]) = {
    import TMManip.enlarge

    require(valid(m1) && valid(m2))
    val max = Math.max(numTapes(m1), Math.max(numTapes(m2), numTapes(m3)))
    val mm1 = if (numTapes(m1) == max) m1 else enlarge(m1, max)
    val mm2 = if (numTapes(m2) == max) m2 else enlarge(m2, max)
    val mm3 = if (numTapes(m3) == max) m3 else enlarge(m3, max)

    (mm1, mm2, mm3)
  }
}
