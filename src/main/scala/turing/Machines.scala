package turing

import turing.ControlFlow.nextBlank

object Machines {
  import TuringMachine._

  def initStates[Q](n: Int, next: () => Q): List[NonHalt[Q]] =
    (1 to n).map(_ => NonHalt(next())).toList

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
    val List(q0, q1, q2) = initStates(3, next)

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

  def erase[A](symbols: Set[A], next: () => Int, special: A): TuringMachine[Int, A] = {
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
    ) + ((init, Blank) -> (Reject, Blank, Stay)))

  def reject[A](init: NonHalt[Int], symbols: Set[A]): TuringMachine[Int, A] =
    TuringMachine(init, symbols.foldLeft[TransTable[Int, A]](Map())((table, symb) =>
      table + ((init, Alph(symb)) -> (Reject, Alph(symb), Stay))
    ) + ((init, Blank) -> (Reject, Blank, Stay)))

  object Binary {
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
    /** `o` is the symbol used to represent 1
     * input: Δ1^mΔ1^n for some m, n >= 0
     *        ^
     * output: Δ1^(m + n)
     *         ^
     */
    def add[A](o: A, next: () => Int): TuringMachine[Int, A] = {
      val List(q0, q1, q2, q3, q4, q5, q6, q7, q8) = initStates(9, next)
      val one = Alph(o)

      val table: TransTable[Int, A] = Map(
        (q0, Blank) -> (q1, Blank, Right), 

        (q1, one) -> (q1, one, Right),
        (q1, Blank) -> (q2, Blank, Right),
    
        (q2, Blank) -> (q8, Blank, Left),
        (q8, Blank) -> (q3, Blank, Left),
        (q2, one) -> (q4, one, Left),

        (q3, one) -> (q3, one, Left),
        (q3, Blank) -> (Accept, Blank, Stay),

        (q4, Blank) -> (q5, one, Right),
        
        (q5, one) -> (q5, one, Right),
        (q5, Blank) -> (q6, Blank, Left),
        
        (q6, one) -> (q7, Blank, Left),

        (q7, one) -> (q7, one, Left),
        (q7, Blank) -> (Accept, Blank, Stay)
      )

      TuringMachine(q0, table)
    }

    /**
     * input: Δ1^mΔ1^n for some m >= n >= 0
     *        ^
     * output: Δ1^(m - n)
     *         ^
     */
    def sub[A](o: A, special: A, next: () => Int): TuringMachine[Int, A] = {
      val List(q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12) = initStates(13, next)
      val one = Alph(o)
      val a = Alph(special)

      val table: TransTable[Int, A] = Map(
        (q0, Blank) -> (q1, Blank, Right),
        (q1, one) -> (q1, one, Right),
        (q1, Blank) -> (q2, Blank, Right),

        (q2, Blank) -> (q3, Blank, Left),
        (q2, one) -> (q5, one, Right),

        (q3, Blank) -> (q4, Blank, Left),

        (q4, one) -> (q4, one, Left),
        (q4, Blank) -> (Accept, Blank, Stay),

        (q5, one) -> (q5, one, Right),
        (q5, Blank) -> (q6, Blank, Left),
        
        (q6, one) -> (q7, a, Left),
        (q6, Blank) -> (q11, a, Right),

        (q7, one) -> (q7, one, Left),
        (q7, Blank) -> (q8, Blank, Left),
        
        (q8, a) -> (q8, a, Left),
        (q8, one) -> (q9, a, Right),

        (q9, a) -> (q9, a, Right),
        (q9, Blank) -> (q10, Blank, Right),

        (q10, one) -> (q10, one, Right),
        (q10, a) -> (q6, a, Left),

        (q11, a) -> (q11, a, Right),
        (q11, Blank) -> (q12, Blank, Left),

        (q12, a) -> (q12, Blank, Left),
        (q12, one) -> (q12, one, Left),
        (q12, Blank) -> (Accept, Blank, Stay)
      )

      TuringMachine(q0, table)
    }

    object MulImpl {
      def delBack[A](o: A, next: () => Int): TuringMachine[Int, A] = {
        val q = NonHalt(next())
        TuringMachine(q, Map(
          (q, Blank) -> (q, Blank, Left),
          (q, Alph(o)) -> (Accept, Blank, Stay)
        ))
      }

      /**
       * input:  Δ11111Δ111111
       *         ^
       * output: #1111ΔΔ111111#
       *              ^
       */
      def init[A](o: A, hash: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, q1, q2, q3, qnb1, qnb2, qpb) = initStates(7, next)
        val nb1 = ControlFlow.nextBlank(Set(o, hash), next)
        val nb2 = ControlFlow.nextBlank(Set(o, hash), next)
        val pb = ControlFlow.prevBlank(Set(o, hash), next)

        val h = Alph(hash)
        val one = Alph(o)

        val m1 = TuringMachine(q0, Map(
          (q0, Blank) -> (qnb1, h, Right),
          (q1, Blank) -> (qpb, h, Left),
          (q2, Blank) -> (q3, Blank, Left),
          (q3, one) -> (Accept, Blank, Stay)
        ))
        val m2 = ControlFlow.insertMachine(m1, nb1, qnb1, qnb2, Reject)
        val m3 = ControlFlow.insertMachine(m2, nb2, qnb2, q1, Reject)
        val m4 = ControlFlow.insertMachine(m3, pb, qpb, q2, Reject)
        m4
      }

      /**
       * if input:  #Δ...
       *             ^
       * then
       *    output: #Δ... (and reject)
       *             ^
       * if input:  #1111Δ...
       *                 ^
       * then
       *    output: #1111Δ... (and accept)
       *                 ^
       */
      def emptyBehind[A](o: A, special: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0) = initStates(1, next)
        TuringMachine(q0, Map(
          (q0, Blank) -> (q0, Blank, Left),
          (q0, Alph(o)) -> (Reject, Alph(o), Right),
          (q0, Alph(special)) -> (Accept, Alph(special), Right),
        ))
      }

      /**
       * input:  Δ...Δ..., (where ... ∈ `symbols`)
       *         ^
       * output: Δ...#... ^
       */
      def appendSpecial[A](symbols: Set[A], special: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, q1, qnb, qpb) = initStates(4, next)
        val nb = ControlFlow.nextBlank(symbols + special, next)
        val pb = ControlFlow.prevBlank(symbols + special, next)

        val m1 = TuringMachine(q0, Map(
          (q0, Blank) -> (qnb, Blank, Stay),
          (q1, Blank) -> (qpb, Alph(special), Stay)
        ))
        val m2 = ControlFlow.insertMachine(m1, nb, qnb, q1, Reject)
        val m3 = ControlFlow.insertMachine(m2, pb, qpb, Accept, Reject)
        m3
      }

      /**
       * input:  Δ...s... (where ... ∈ `symbols` and s = `symbol`)
       *         ^
       * output: Δ...s...
       *             ^
       */
      def find[A](symbols: Set[A], symbol: A, next: () => Int): TuringMachine[Int, A] = {
        val ss = symbols - symbol
        val List(q0) = initStates(1, next)
        TuringMachine(q0, Map(
          (q0, Blank) -> (q0, Blank, Right),
          (q0, Alph(symbol)) -> (Accept, Alph(symbol), Stay)
        ) ++ ss.map(s => (q0, Alph(s)) -> (q0, Alph(s), Right)).toMap)
      }

      /**
       * input:  ...s...Δ... (where ... ∈ `symbols` and s = `symbol`)
       *                ^
       * output: ...s...Δ...
       *            ^
       */
      def findReverse[A](symbols: Set[A], symbol: A, next: () => Int): TuringMachine[Int, A] =
        TMManip.mirror(find(symbols, symbol, next))

      /**
       * input:  Δ111#  (where # = `symbol`)
       *             ^
       * output: Δ111#111
       *             ^
       */ 
      def copyAcross[A](o: A, symbol: A, special: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, q1, q2, q3, q4, q5, q6) = initStates(7, next)
        val hash = Alph(symbol)
        val a = Alph(special)
        val one = Alph(o)

        TuringMachine(q0, Map(
          (q0, hash) -> (q1, hash, Left),
          (q1, one) -> (q2, a, Right),
          (q1, Blank) -> (q6, Blank, Right),
          (q2, a) -> (q2, a, Right),
          (q2, hash) -> (q3, hash, Right),
          (q3, one) -> (q3, one, Right),
          (q3, Blank) -> (q4, one, Left),
          (q4, one) -> (q4, one, Left),
          (q4, hash) -> (q5, hash, Left),
          (q5, a) -> (q5, a, Left),
          (q5, one) -> (q1, one, Stay),
          (q5, Blank) -> (q1, Blank, Stay),
          (q6, a) -> (q6, one, Right),
          (q6, hash) -> (Accept, hash, Stay)
        ))
      }

      /**
       * input:  #1111ΔΔΔΔ111Δ111Δ111#
       *              ^ 
       * output: #111ΔΔΔΔΔ111Δ111Δ111Δ111#
       *             ^
       */
      def mulIterate[A](o: A, hash: A, special: A, next: () => Int): TuringMachine[Int, A] = {
        val symbols = Set(o, hash, special)

        val List(q0, q1, q2, qdelback, qfindhash, qcopyacross, qnb1, qnb2, qfindhashrev) = initStates(9, next)

        val delback = delBack(o, next)
        val findhash = find(symbols, hash, next)
        val copyacross = copyAcross(o, hash, special, next)
        val nb1 = ControlFlow.nextBlank(symbols, next)
        val nb2 = ControlFlow.nextBlank(symbols, next)
        val findhashrev = findReverse(symbols, hash, next)

        val m1 = TuringMachine(q0, Map(
          (q0, Blank) -> (qdelback, Blank, Stay),
          (q1, Alph(hash)) -> (qnb1, Blank, Right),
          (q2, Blank) -> (qfindhashrev, Alph(hash), Left),
        ))

        val m2 = ControlFlow.insertMachine(m1, delback, qdelback, qfindhash, Reject)
        val m3 = ControlFlow.insertMachine(m2, findhash, qfindhash, qcopyacross, Reject)
        val m4 = ControlFlow.insertMachine(m3, copyacross, qcopyacross, q1, Reject)
        val m5 = ControlFlow.insertMachine(m4, nb1, qnb1, q2, Reject)
        val m6 = ControlFlow.insertMachine(m5, findhashrev, qfindhashrev, qnb2, Reject)
        ControlFlow.insertMachine(m6, nb2, qnb2, Accept, Reject)
      }

      /**
       * input:  #111Δ111Δ111#...
       *         ^
       * output: #111111111#ΔΔ... 
       *         ^
       */
      def combineGroups[A](o: A, hash: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, qfindhash, q1, q2, q3, q4) = initStates(6, next)
        val findhash = find(Set(o), hash, next)
        val one = Alph(o)
        val h = Alph(hash)
        
        val m1 = TuringMachine(q0, Map(
          (q0, h) -> (qfindhash, h, Right),
          (q1, h) -> (q2, h, Left),
          (q2, one) -> (q2, one, Left),
          (q2, h) -> (Accept, h, Stay),
          (q2, Blank) -> (q3, one, Right),
          (q3, one) -> (q3, one, Right),
          (q3, h) -> (q4, Blank, Left),
          (q4, one) -> (q2, h, Left)
        ))
        ControlFlow.insertMachine(m1, findhash, qfindhash, q1, Reject)
      }

      /**
       * input:  #ΔΔΔΔ11111#...
       *             ^
       * output: #11111#...
       *         ^
       */
      def trimLeft[A](o: A, hash: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, q1, q2, q3, qfindhash) = initStates(5, next)
        val findhash = find(Set(o), hash, next)
        val h = Alph(hash)
        val one = Alph(o)

        val m1 = TuringMachine(q0, Map(
          (q0, Blank) -> (qfindhash, one, Right),
          (q0, h) -> (Accept, h, Stay),
          (q1, h) -> (q2, Blank, Left),
          (q2, one) -> (q3, h, Left),
          (q3, one) -> (q3, one, Left),
          (q3, Blank) -> (q0, Blank, Stay),
          (q3, h) -> (q0, h, Stay)
        ))
        ControlFlow.insertMachine(m1, findhash, qfindhash, q1, Reject)
      }

      /**
       * input:  ΔΔΔΔΔΔΔΔs.. (where s ∈ `stopSymbols`)
       *         ^
       * output: ΔΔΔΔΔΔΔΔs..
       *                ^
       */
      def lastBlank[A](stopSymbols: Set[A], next: () => Int): TuringMachine[Int, A] = {
        val List(q0) = initStates(1, next)
        TuringMachine(q0, 
          stopSymbols.map(s => (q0, Alph(s)) -> (Accept, Alph(s), Left)).toMap +
          ((q0, Blank) -> (q0, Blank, Right))
        )
      }

      /**
       * input:  #ΔΔΔΔΔ111Δ111Δ111#
       *          ^
       * output: Δ111111111
       */
      def mulFinish[A](o: A, hash: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, q1, q2, q3, q4, qlastblank, qcombinegroups, qtrimleft, qfindhash, qpb) = initStates(10, next)
        val lastblank = lastBlank(Set(o), next)
        val combinegroups = combineGroups(o, hash, next)
        val trimleft = trimLeft(o, hash, next)
        val findhash = find(Set(o), hash, next)
        val pb = ControlFlow.prevBlank(Set(o, hash), next)
        val one = Alph(o)
        val h = Alph(hash)

        val m1 = TuringMachine(q0, Map(
          (q0, Blank) -> (qlastblank, Blank, Right),
          (q1, Blank) -> (qcombinegroups, h, Stay),
          (q2, h) -> (qtrimleft, Blank, Left),
          (q3, h) -> (qfindhash, Blank, Right),
          (q4, h) -> (qpb, Blank, Left)
        ))
        val m2 = ControlFlow.insertMachine(m1, lastblank, qlastblank, q1, Reject)
        val m3 = ControlFlow.insertMachine(m2, combinegroups, qcombinegroups, q2, Reject)
        val m4 = ControlFlow.insertMachine(m3, trimleft, qtrimleft, q3, Reject)
        val m5 = ControlFlow.insertMachine(m4, findhash, qfindhash, q4, Reject)
        val m6 = ControlFlow.insertMachine(m5, pb, qpb, Accept, Reject)
        m6
      }

      /**
       * input:  Δ1^mΔ1^n for some m, n > 0
       *         ^
       * output: Δ1^(m + n)
       */
      def positiveMultiply[A](o: A, hash: A, special: A, next: () => Int): TuringMachine[Int, A] = {
        val List(q0, q1, qinit, qemptybehind, qmulfinish, qmuliterate) = initStates(6, next)
        val initt = init(o, hash, next)
        val emptybehind = emptyBehind(o, hash, next)
        val mulfinish = mulFinish(o, hash, next)
        val muliterate = mulIterate(o, hash, special, next)

        println(s"qemptybehind: $qemptybehind")

        val m1 = TuringMachine[Int, A](q0, Map(
          (q0, Blank) -> (qinit, Blank, Stay),
          (q1, Blank) -> (qemptybehind, Blank, Stay)
        ))
        val m2 = ControlFlow.insertMachine(m1, initt, qinit, qemptybehind, Reject)
        val m3 = ControlFlow.insertMachine(m2, emptybehind, qemptybehind, qmulfinish, qmuliterate)
        val m4 = ControlFlow.insertMachine(m3, mulfinish, qmulfinish, Accept, Reject)
        val m5 = ControlFlow.insertMachine(m4, muliterate, qmuliterate, q1, Reject)
        m5
      }
    }
  }
}
