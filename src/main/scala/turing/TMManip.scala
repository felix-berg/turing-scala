package turing

object TMManip {
  import TMUtil.*
  import TuringMachine.*
  
  /** make TM `m` work on the tape indices specified by `tapes`, on the resulting `nTapes`-taped TM */
  def workOn[Q, A](m: MultiMachine[Q, A], tapes: List[Int], nTapes: Int): MultiMachine[Q, A] = {
    require(valid(m))
    require(tapes.forall(t => t <= nTapes))
    require(tapes.length == numTapes(m))

    def convertList[T](list: List[T], noelm: T): List[T] = {
      val neww = (0 until nTapes).map(_ => noelm).toList
      tapes.zip(list).foldLeft(neww) {
        case (nw, (tapeidx, elm)) => nw.updated(tapeidx, elm)
      }
    }

    MultiMachine[Q, A](m.init, m.transitions.map {
      case (q1, ss1) -> (q2, ss2, dirs) =>
        (q1, convertList(ss1, Blank)) -> (q2, convertList(ss2, Blank), convertList(dirs, Stay))
    })
  }

  def workOn[Q, A](m: TuringMachine[Q, A], tape: Int, nTapes: Int): MultiMachine[Q, A] = {
    val blanks: List[TapeAlph[A]] = (0 until nTapes).map(_ => Blank).toList
    val stays: List[Direction] = (0 until nTapes).map(_ => Stay).toList

    MultiMachine(
      m.init, m.transitions.map {
        case (q1, s1) -> (q2, s2, dir) =>
          (q1, blanks.updated(tape, s1)) -> (q2, blanks.updated(tape, s2), stays.updated(tape, dir))
      })
  }

  def enlarge[Q, A](m: MultiMachine[Q, A], nTapes: Int): MultiMachine[Q, A] =
    workOn(m, (0 until numTapes(m)).toList, nTapes)

  def enlarge[Q, A](m: TuringMachine[Q, A], nTapes: Int): MultiMachine[Q, A] =
    workOn(m, 0, nTapes)

  def mirror[Q, A](m: TuringMachine[Q, A]): TuringMachine[Q, A] = 
    TuringMachine(m.init, m.transitions.map {
      case (q1, s1) -> (q2, s2, Right) =>
        (q1, s1) -> (q2, s2, Left)
      case (q1, s1) -> (q2, s2, Left) =>
        (q1, s1) -> (q2, s2, Right)
      case t => t
    })
}
