package turing.machines

object SimpleOps {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.TMManip._
  import turing.ControlFlow._

  /**
   * input: Δx for some string x ∈ `symbols`
   *        ^
   * output: ΔxΔx
   *         ^
   */
  def copy[A](symbols: Set[A], alternate: A => A, next: () => Int): TuringMachine[Int, A] = {
    val nextState = () => NonHalt(next())
    val List(q0, q1, q2) = initStates(3, next)
    val empty: TransTable[Int, A] = Map()

    val table = Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, Blank) -> (q2, Blank, Left),
      (q2, Blank) -> (Accept, Blank, Stay)
    ) ++ symbols.foldLeft(empty)((tt, aa) => {
      val a = Alph(aa)
      val A = Alph(alternate(aa))

      val List(qa1, qa2, qa3) = initStates(3, next)
      tt ++ Map(
        (q1, a) -> (qa1, A, Right),
        (q2, A) -> (q2, a, Left),
        (qa1, Blank) -> (qa2, Blank, Right),
        (qa2, Blank) -> (qa3, a, Left),
        (qa3, A) -> (q1, A, Right),
        (qa3, Blank) -> (qa3, Blank, Left)
      ) ++ symbols.foldLeft(empty)((ttt, xx) => {
        val x = Alph(xx)
        val X = Alph(alternate(xx))

        ttt ++ Map(
          (qa1, x) -> (qa1, x, Right),
          (qa2, x) -> (qa2, x, Right),
          (qa3, x) -> (qa3, x, Left)
        )
      })
    })

    TuringMachine(q0, table)
  }

  /**
   * input:  ΔxΔy (where x, y ∈ `symbols`)
   *         ^
   * output:
   *  - ΔxΔy and accept (if x == y)
   *    ^
   *  - ΔxΔy and reject (if x != y)
   *    ^
   */
  def equal[A](symbols: Set[A], alternate: A => A, next: () => Int): TuringMachine[Int, A] = {
    val List(q0, q1, q2, q3, q4, q5, q6, qc1, qc2) = initStates(9, next)
    val ss = symbols.map(s => Alph(s))
    val upper = (c: Alph[A]) => Alph(alternate(c._1))
    TuringMachine(q0,
      Map(
        (q0, Blank) -> (q1, Blank, Right),
        (q1, Blank) -> (q2, Blank, Right),
        (q2, Blank) -> (q3, Blank, Left),
        (q3, Blank) -> (q4, Blank, Left),
        (q4, Blank) -> (Accept, Blank, Stay),
        (q5, Blank) -> (q6, Blank, Left),
        (q6, Blank) -> (Reject, Blank, Stay),
        (qc1, Blank) -> (qc2, Blank, Left),
        (qc2, Blank) -> (Reject, Blank, Stay)
      ) ++ ss.map(x => {
        val List(qx1, qx2, qx3, qx4) = initStates(4, next)
        List(
          (q1, x) -> (qx1, upper(x), Right),
          (qx2, x) -> (qx3, upper(x), Left),
          (qx1, Blank) -> (qx2, Blank, Right),
          (qx2, Blank) -> (qc1, Blank, Left),
          (qx3, Blank) -> (qx4, Blank, Left),
          (qx4, upper(x)) -> (q1, upper(x), Right),
        ) ++ ss.map(y => 
          List(
            (qx1, y) -> (qx1, y, Right),
            (qx2, upper(y)) -> (qx2, upper(y), Right),
            (qx3, upper(y)) -> (qx3, upper(y), Left),
            (qx4, y) -> (qx4, y, Left)
          ) ++ (if (y == x) Nil else List((qx2, y) -> (qc1, y, Left)))
        ).flatten
      }).flatten ++ ss.map(y => List(
        (qc1, upper(y)) -> (qc1, y, Left),
        (qc2, y) -> (qc2, y, Left),
        (qc2, upper(y)) -> (qc2, y, Left),
        (q2, upper(y)) -> (q2, upper(y), Right),
        (q2, y) -> (q5, y, Left),
        (q3, upper(y)) -> (q3, y, Left),
        (q4, upper(y)) -> (q4, y, Left),
        (q5, upper(y)) -> (q5, y, Left),
        (q6, upper(y)) -> (q6, y, Left)
      )).flatten
    )
  }

  def replaceSymbol[A](from: TapeAlph[A], to: TapeAlph[A], next: () => Int): TuringMachine[Int, A] = {
    val List(q0) = initStates(1, next)
    TuringMachine(q0, Map(
      (q0, from) -> (Accept, to, Stay)
    ))
  }

  /**
   * input:  Δx
   *         ^
   * output: Δx (reject iff x = "")
   *         ^
   */
  def isEmpty[A](symbols: Set[A], next: () => Int): TuringMachine[Int, A] = {
    val (q0, q1) = (NonHalt(next()), NonHalt(next()))
    TuringMachine(q0, Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, Blank) -> (Accept, Blank, Left)
    ) ++ symbols.foldLeft[TransTable[Int, A]](Map())((tt, symb) =>
      tt + ((q1, Alph(symb)) -> (Reject, Alph(symb), Left))
    ))
  }

  def nonEmpty[A](symbols: Set[A], next: () => Int): TuringMachine[Int, A] = {
    val (q0, q1) = (NonHalt(next()), NonHalt(next()))
    TuringMachine(q0, Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, Blank) -> (Reject, Blank, Left)
    ) ++ symbols.foldLeft[TransTable[Int, A]](Map())((tt, symb) =>
      tt + ((q1, Alph(symb)) -> (Accept, Alph(symb), Left))
    ))
  }

  def erase[A](symbols: Set[A], special: A, next: () => Int): TuringMachine[Int, A] = {
    require(!symbols.contains(special))
    val List(q0, q1, q2) = initStates(3, next)
    val table: TransTable[Int, A] = Map(
      (q0, Blank) -> (q1, Alph(special), Right),
      (q1, Blank) -> (q2, Blank, Left),
      (q2, Blank) -> (q2, Blank, Left),
      (q2, Alph(special)) -> (Accept, Blank, Stay)
    )

    TuringMachine(q0, symbols.foldLeft(table)((tt, symb) => tt + ((q1, Alph(symb)) -> (q1, Blank, Right))))
  }

  def eraseN[A](n: Int, symbols: Set[A], special: A, next: () => Int): TuringMachine[Int, A] = 
    if (n == 0) 
      return accept(NonHalt(next()), Set()) 
    else {
      val goToBack = (1 to n - 1).map(_ => nextBlank(symbols, next)).toList
      val eraseAll = (1 to n).map(_ => List(erase(symbols, special, next), prevBlank(symbols, next)))
        .toList.flatten.take(2 * n - 1)
      sequence(goToBack ++ eraseAll)
    }
  

  def accept[A](init: NonHalt[Int], symbols: Set[A]): TuringMachine[Int, A] =
    TuringMachine(init, symbols.foldLeft[TransTable[Int, A]](Map())((table, symb) =>
      table + ((init, Alph(symb)) -> (Accept, Alph(symb), Stay))
    ) + ((init, Blank) -> (Accept, Blank, Stay)))

  def reject[A](init: NonHalt[Int], symbols: Set[A]): TuringMachine[Int, A] =
    TuringMachine(init, symbols.foldLeft[TransTable[Int, A]](Map())((table, symb) =>
      table + ((init, Alph(symb)) -> (Reject, Alph(symb), Stay))
    ) + ((init, Blank) -> (Reject, Blank, Stay)))

  /**
   * input:  Δ
   *         ^
   * output: Δs^n   (where s = `symbol`)
   */
  def nSymbols[A](symbol: A, n: Int, next: () => Int): TuringMachine[Int, A] = {
    if (n == 0) {
      val List(q0) = initStates(1, next)
      accept(q0, Set())
    } else {
      val one = Alph(symbol)
      // TODO: reduce number of states by doubling log2(n) times
      val qi :: qf :: qs = initStates(n + 3, next): @unchecked
      
      val qe = qs(qs.length - 1)

      val pairs = qs.take(qs.length - 1).zip(qs.tail)
      val middleTransitions = pairs.map((qf, qt) => (qf, Blank) -> (qt, one, Right))

      TuringMachine(qi, middleTransitions.toMap ++ Map(
        (qi, Blank) -> (qs.head, Blank, Right),
        (qe, Blank) -> (qf, Blank, Left),
        (qf, one) -> (qf, one, Left),
        (qf, Blank) -> (Accept, Blank, Stay)
      ))
    }
  }

  /**
   * input:  Δ
   *         ^
   * output: Δx  (where x = `str`)
   *         ^
   */
  def write[A](str: List[A], next: () => Int): TuringMachine[Int, A] = 
    if (str.isEmpty) accept(NonHalt(next()), Set()) else {
      val sstr = str.map(s => Alph(s))

      val q0 :: qss = initStates(str.length + 3, next): @unchecked
      val qs = qss.take(str.length)
      val qe = qss(qss.length - 2)
      val qf = qss(qss.length - 1)
      val qlast = qss(qss.length - 3) // qn or q0 (if str = "")

      val q1qn_1 = qs // q1 -> qn
      val q2qn = qs.tail ++ List(qe) // q2 -> qe
      assert(q1qn_1.length == str.length && q2qn.length == str.length)

      TuringMachine(q0, Map(
        (q0, Blank) -> (qss.head, Blank, Right),
        (qe, Blank) -> (qf, Blank, Left),
        (qf, Blank) -> (Accept, Blank, Stay)
      ) ++ q1qn_1.zip(q2qn).zip(sstr).map {
        case ((qi, qip1), si) =>
          (qi, Blank) -> (qip1, si, Right)
      } ++ sstr.toSet.map(s => (qf, s) -> (qf, s, Left)))
    }

  def findSymbol[A](symbols: Set[A], target: A, next: () => Int): TuringMachine[Int, A] = {
    require(!symbols.contains(target))
    val List(q0, q1) = initStates(2, next)
    TuringMachine(q0, Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, Blank) -> (q1, Blank, Right),
      (q1, Alph(target)) -> (Accept, Alph(target), Stay),
      (q0, Alph(target)) -> (q1, Alph(target), Right)
    ) ++ symbols.map(s => List(
      (q0, Alph(s)) -> (q1, Alph(s), Right),
      (q1, Alph(s)) -> (q1, Alph(s), Right)
    )).flatten)
  }

  def findSymbolRev[A](symbols: Set[A], target: A, next: () => Int): TuringMachine[Int, A] = 
    mirror(findSymbol(symbols, target, next))

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
    mirror(nextBlank(symbols, next))

  def nextBlankWithStop[A](symbols: Set[A], stop: A, next: () => Int): TuringMachine[Int, A] = {
    val (q0, q1) = (NonHalt(next()), NonHalt(next()))
    val table = (symbols - stop).map(s => Alph(s))
      .foldLeft[TransTable[Int, A]](Map())((tt, s) => tt +
        ((q0, s) -> (q1, s, Right)) +
        ((q1, s) -> (q1, s, Right))
      ) + ((q0, Blank) -> (q1, Blank, Right))
        + ((q1, Blank) -> (Accept, Blank, Stay))
        + ((q0, Alph(stop)) -> (Reject, Alph(stop), Stay))
        + ((q1, Alph(stop)) -> (Reject, Alph(stop), Stay))

    TuringMachine(q0, table)
  }

  def prevBlankWithStop[A](symbols: Set[A], stop: A, next: () => Int): TuringMachine[Int, A] = 
    mirror(nextBlankWithStop(symbols, stop, next))
}
