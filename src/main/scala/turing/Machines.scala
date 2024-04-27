package turing

object Machines {
  import TuringMachine._

  def copy[A](symbols: Set[A], alternate: A => A, next: () => Int): TuringMachine[Int, A] = {
    val nextState = () => NonHalt(next())
    val (q0, q1, q2) = (nextState(), nextState(), nextState())
    val empty: TransTable[Int, A] = Map()

    val table = Map(
      (q0, Blank) -> (q1, Blank, Right),
      (q1, Blank) -> (q2, Blank, Left),
      (q2, Blank) -> (Accept, Blank, Stay)
    ) ++ symbols.foldLeft(empty)((tt, aa) => {
      val a = Alph(aa)
      val A = Alph(alternate(aa))

      val (qa1, qa2, qa3) = (nextState(), nextState(), nextState())
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

  def multiEq[A](symbols: Set[A], next: () => Int): MultiMachine[Int, A] = {
    val (q0, q1, q2) = (NonHalt(next()), NonHalt(next()), NonHalt(next()))

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
    val (q0, q1, q2) = (NonHalt(next()), NonHalt(next()), NonHalt(next()))
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

  def erase[A](symbols: Set[A], next: () => Int, special: A): TuringMachine[Int, A] = {
    require(!symbols.contains(special))
    val (q0, q1, q2) = (NonHalt(next()), NonHalt(next()), NonHalt(next()))
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
    ) + ((init, Blank) -> (Reject, Blank, Stay)))

  def reject[A](init: NonHalt[Int], symbols: Set[A]): TuringMachine[Int, A] =
    TuringMachine(init, symbols.foldLeft[TransTable[Int, A]](Map())((table, symb) =>
      table + ((init, Alph(symb)) -> (Reject, Alph(symb), Stay))
    ) + ((init, Blank) -> (Reject, Blank, Stay)))

  object Binary {
    def incr[A](zero: A, one: A, next: () => Int): TuringMachine[Int, A] = {
      val (q0, q1, q2) = (NonHalt(next()), NonHalt(next()), NonHalt(next()))
      val (o, z) = (Alph(one), Alph(zero))

      val table: TransTable[Int, A] = Map()
        + ((q0, Blank) -> (q1, Blank, Right))
        + ((q1, o) -> (q1, z, Right))
        + ((q1, z) -> (q2, o, Left))
        + ((q1, Blank) -> (q2, o, Left))
        + ((q2, o) -> (q2, o, Left))
        + ((q2, z) -> (q2, z, Left))
        + ((q2, Blank) -> (Accept, Blank, Stay))

      TuringMachine(q0, table)
    }

    def decr[A](zero: A, one: A, next: () => Int): TuringMachine[Int, A] = {
      val (q0, q1, q2, q3, q4) = (NonHalt(next()), NonHalt(next()), NonHalt(next()), NonHalt(next()), NonHalt(next()))
      val (o, z) = (Alph(one), Alph(zero))

      val table: TransTable[Int, A] = Map(
        (q0, Blank) -> (q1, Blank, Right),
        (q1, z) -> (q1, o, Right),
        (q1, o) -> (q2, z, Right),

        (q2, z) -> (q3, z, Left),
        (q2, o) -> (q3, o, Left),
        (q2, Blank) -> (q4, Blank, Left),

        (q3, o) -> (q3, o, Left),
        (q3, z) -> (q3, z, Left),
        (q3, Blank) -> (Accept, Blank, Stay),

        (q4, z) -> (q3, Blank, Left)
      )

      TuringMachine(q0, table)
    }

    def add[A](zero: A, one: A, next: () => Int): MultiMachine[Int, A] = {
      import ControlFlow.*
      import TMManip.*
      import TMUtil.*

      val incr0 = workOn(incr(zero, one, next), 0, 2)
      val decr1 = workOn(decr(zero, one, next), 1, 2)
      val nonEmpty1 = workOn(nonEmpty(Set(zero, one), next), 1, 2)

      assert(combinable(incr0, decr1) && combinable(decr1, nonEmpty1))

      whileLoop(nonEmpty1, sequence(incr0, decr1))
    }

    def sub[A](zero: A, one: A, next: () => Int): MultiMachine[Int, A] = {
      import ControlFlow.*
      import TMManip.*
      import TMUtil.*

      val decr0 = workOn(decr(zero, one, next), 0, 2)
      val decr1 = workOn(decr(zero, one, next), 1, 2)
      val nonEmpty1 = workOn(nonEmpty(Set(zero, one), next), 1, 2)

      assert(combinable(decr0, decr1) && combinable(decr1, nonEmpty1))

      whileLoop(nonEmpty1, sequence(decr0, decr1))
    }

    def mult[A](zero: A, one: A, szero: A, sone: A, next: () => Int): MultiMachine[Int, A] = {
      import ControlFlow.*
      import TMUtil.*
      import TMManip.*

      val next = { var n = 0; () => { n += 1; n } }
      val set = Set(zero, one)

      val special = szero // use szero as '#'
      val alternate = (s: A) => if (s == zero) szero else sone

      val nTapes = 4

      val copyDown02 = workOn(copyDown(set, next), List(0, 2), nTapes)
      val copyDown23 = workOn(copyDown(set, next), List(2, 3), nTapes)
      val add03      = workOn(add(zero, one, next), List(0, 3), nTapes)

      val decr1      = workOn(decr(zero, one, next), 1, nTapes)
      val erase0     = workOn(erase(set, next, special), 0, nTapes)
      val erase2     = workOn(erase(set, next, special), 2, nTapes)
      val erase3     = workOn(erase(set, next, special), 3, nTapes)

      val nonEmpty1  = workOn(nonEmpty(set, next), 1, nTapes)

      sequence(copyDown02 :: erase0 ::
        whileLoop(nonEmpty1, sequence(
          decr1 :: copyDown23 :: add03 :: Nil
        ))
      :: erase2 :: erase3 :: Nil)
    }
  }

  object Numbers {
    def add[A](s: A, next: () => Int): TuringMachine[Int, A] = {
      val (q0, q1, q2, q3, q4) = (NonHalt(next()), NonHalt(next()), NonHalt(next()), NonHalt(next()), NonHalt(next()))
      val one = Alph(s)
      val table: TransTable[Int, A] = Map(
        (q0, Blank) -> (q1, Blank, Right), 
        (q1, one) -> (q1, one, Right),
        (q1, Blank) -> (q2, one, Right),
        (q2, one) -> (q2, one, Right),
        (q2, Blank) -> (q3, Blank, Left),
        (q3, one) -> (q4, Blank, Left),
        (q4, one) -> (q4, one, Left),
        (q4, Blank) -> (Accept, Blank, Stay)
      )

      TuringMachine(q0, table)
    }
  }
}
