package turing.machines

object SimpleOps {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.TMManip._

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
    val nextState = () => NonHalt(next())

    val lower = (x: A) => Alph(x)
    val upper = (x: A) => Alph(alternate(x))

    val (q0, q1, q2, q3, q4) = (nextState(), nextState(), nextState(), nextState(), nextState())

    val table: TransTable[Int, A] = Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, Blank) -> (q2, Blank, Right),
      (q2, Blank) -> (q3, Blank, Left),
      (q3, Blank) -> (q4, Blank, Left),
      (q4, Blank) -> (Accept, Blank, Stay)
    )

    val table1 = symbols.foldLeft(table)((tt, x) => tt +
      ((q2, upper(x)) -> (q2, lower(x), Right)) +
      ((q3, lower(x)) -> (q3, lower(x), Left)) +
      ((q4, upper(x)) -> (q4, lower(x), Left))
    )

    val table2 = symbols.foldLeft(table1)((tt, a) => {
      val (qa1, qa2, qa3, qa4) = (nextState(), nextState(), nextState(), nextState())

      val empty: TransTable[Int, A] = Map()

      tt +
        ((q1, lower(a)) -> (qa1, upper(a), Right)) +
        ((qa1, Blank) -> (qa2, Blank, Right)) +
        ((qa2, lower(a)) -> (qa3, upper(a), Left)) +
        ((qa3, Blank) -> (qa4, Blank, Left)) +
        ((qa4, upper(a)) -> (q1, upper(a), Right)) ++
        symbols.foldLeft(empty)((ttt, x) => ttt +
          ((qa1, lower(x)) -> (qa1, lower(x), Right)) +
          ((qa2, upper(x)) -> (qa2, upper(x), Right)) +
          ((qa3, upper(x)) -> (qa3, upper(x), Left)) +
          ((qa4, lower(x)) -> (qa4, lower(x), Left))
        )
    })

    TuringMachine[Int, A](q0, table2)
  }

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
}
