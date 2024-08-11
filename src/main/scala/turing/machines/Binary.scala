package turing.machines

object Binary {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.ControlFlow._
  import turing.TMUtil._
  import turing.TMManip._
  import SimpleOps._
  import MultiTapeOps._

  def incr[A](zero: A, one: A, next: () => Int): TuringMachine[Int, A] = {
    val List(q0, q1, q2) = initStates(3, next)
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
    val List(q0, q1, q2, q3, q4) = initStates(5, next)
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
    val incr0 = workOn(incr(zero, one, next), 0, 2)
    val decr1 = workOn(decr(zero, one, next), 1, 2)
    val nonEmpty1 = workOn(nonEmpty(Set(zero, one), next), 1, 2)

    assert(combinable(incr0, decr1) && combinable(decr1, nonEmpty1))

    whileLoop(nonEmpty1, sequence(incr0, decr1))
  }

  def sub[A](zero: A, one: A, next: () => Int): MultiMachine[Int, A] = {
    val decr0 = workOn(decr(zero, one, next), 0, 2)
    val decr1 = workOn(decr(zero, one, next), 1, 2)
    val nonEmpty1 = workOn(nonEmpty(Set(zero, one), next), 1, 2)

    assert(combinable(decr0, decr1) && combinable(decr1, nonEmpty1))

    whileLoop(nonEmpty1, sequence(decr0, decr1))
  }

  def mult[A](zero: A, one: A, szero: A, sone: A, next: () => Int): MultiMachine[Int, A] = {

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

