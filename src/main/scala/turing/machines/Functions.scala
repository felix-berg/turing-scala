package turing.machines


object Functions {
  import turing.TuringMachine._
  import turing.TMUtil._
  import turing.TMManip
  import turing.ControlFlow
  import turing.machines._

  object Impl {
    /**
     * input:  Δ
     *         ^
     * output: Δp1,p2,...,pkΔ
     *         ^            
     */
    def writeParams[A](params: List[List[A]], comma: A, next: () => Int): TuringMachine[Int, A] = 
      if (params.isEmpty) return SimpleOps.accept(NonHalt(next()), Set()) else {
        val set = params.toSet.flatten
        val writes = params.map(param => List(
          SimpleOps.write[A](param, next), 
          SimpleOps.nextBlank[A](set, next)
        )).flatten.dropRight(1)

        val goBack = (1 to params.size - 1).map(_ => List(
          SimpleOps.replaceSymbol(Blank, Alph(comma), next),
          SimpleOps.prevBlank(set + comma, next)
        )).flatten.toList

        ControlFlow.sequence(writes ++ goBack)
      }


    /**
     * input:  [Δ]
     *          ^
     *         [#x1Δv1Δ...ΔxnΔvnΔ]
     *                          ^
     * output: [Δx1,v1,...,xn,vnΔ] (where `params` = p1, ..., pk, c = `code`)
     *                          ^
     *         [#x1Δv1Δ...ΔxnΔvnΔ]
     *                          ^
     */
    def copyEnv[A](symbols: Set[A], comma: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
      val (cm, hs) = (Alph(comma), Alph(hash))

      val prepareEnv = TMManip.workOn({
        val List(q0, q1, q2, qpb) = initStates(4, next)
        val pb = SimpleOps.prevBlankWithStop(symbols + comma, hash, next)
        val parent = TuringMachine(q0, Map(
          (q0, Blank) -> (qpb, Blank, Stay),
          (q1, Blank) -> (qpb, cm, Stay),
          (q2, hs) -> (Accept, Blank, Stay)
        ))
        ControlFlow.insertMachine(parent, pb, qpb, q1, q2)
      }, 1, 2)
  
      val copyToMain = TMManip.workOn(
        MultiTapeOps.copyDown(symbols + comma, next),
        List(1, 0), 2 // invert direction
      )

      val cleanEnv = TMManip.workOn({
        val List(q0, q1, qnb) = initStates(3, next)
        val nb = SimpleOps.nextBlankWithStop(symbols + hash, comma, next)
        val parent = TuringMachine(q0, Map(
          (q0, Blank) -> (qnb, hs, Stay),
          (q1, cm) -> (qnb, Blank, Stay)
        ))
        ControlFlow.insertMachine(parent, nb, qnb, Accept, q1)
      }, 1, 2)

      val forward = TMManip.workOn(
          SimpleOps.nextBlank(symbols + comma, next),
        0, 2)

      ControlFlow.sequence(List(prepareEnv, copyToMain, cleanEnv, forward))
    }

    /**
     * input:  ...sx1fx2f...fxnΔ (where xi ∈ (`symbols` + Blank)*, s = `stop`, f = `from`)
     *                         ^
     * output: ...sx1Δx2Δ...ΔxnΔ
     *                         ^
     */
    def replaceWithBlankBehindUntil[A](symbols: Set[A], stop: A, from: A, hash: A, next: () => Int): TuringMachine[Int, A] = {
      val ss = symbols.removedAll(List(stop, from, hash))
      val List(q0, q1, q2) = initStates(3, next)
      TuringMachine(q0, Map(
        (q0, Blank) -> (q1, Alph(hash), Left),
        (q1, Blank) -> (q1, Blank, Left),
        (q1, Alph(from)) -> (q1, Blank, Left),
        (q1, Alph(stop)) -> (q2, Alph(stop), Right),
        (q2, Blank) -> (q2, Blank, Right),
        (q2, Alph(hash)) -> (Accept, Blank, Stay)
      ) ++ ss.map(s => List(
        (q1, Alph(s)) -> (q1, Alph(s), Left),
        (q2, Alph(s)) -> (q2, Alph(s), Right)
      )).flatten)
    }

    /**
     * moves over `symbols`, looks for any of the symbols in pairs, branches to given state if found.
     * first symbol is skipped (like `nextBlank`).
     */
    def findOptions[A](symbols: Set[A], pairs: List[(TapeAlph[A], State[Int])], next: () => Int): TuringMachine[Int, A] = {
      val List(q0, q1) = initStates(2, next)
      TuringMachine(q0, Map(
        (q0, Blank) -> (q1, Blank, Right),
        (q1, Blank) -> (q1, Blank, Right),
      ) ++ symbols.map(x => List(
        (q0, Alph(x)) -> (q1, Alph(x), Right),
        (q1, Alph(x)) -> (q1, Alph(x), Right)
      )).flatten ++ pairs.map((a, q) => List(
        (q0, a) -> (q0, a, Right),
        (q1, a) -> (q, a, Stay)
      )).flatten)
    }

    /**
     * input:  [Δp1,p2,...,pkΔa1Δ...ΔakΔ]
     *          ^
     *         [Δ]
     *          ^
     * output: [Δ]
     *          ^
     *         [Δp1Δa1Δ...ΔpkΔakΔ]
     *                          ^
     */
    def loadParamsToEnv[A](namesymbols: Set[A], valuesymbols: Set[A], comma: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
      val prepare = TMManip.workOn({
        ControlFlow.sequence(List(
          SimpleOps.replaceSymbol(Blank, Alph(hash), next),
          SimpleOps.nextBlank(namesymbols + comma + hash, next),
          SimpleOps.replaceSymbol(Blank, Alph(hash), next),
          SimpleOps.findSymbolRev(namesymbols + comma, hash, next)
        ))
      }, 0, 2)


      val loopOnBlank = {
        val moveParam = {
          val prepareMove = TMManip.workOn(ControlFlow.sequence(List(
            SimpleOps.findSymbolRev(namesymbols, hash, next),
            SimpleOps.replaceSymbol(Alph(hash), Blank, next)
          )), 0, 2)
          val move = MultiTapeOps.moveDown2(namesymbols, next)
          val afterMove = TMManip.workOn(SimpleOps.replaceSymbol(Blank, Alph(hash), next), 0, 2)
          ControlFlow.sequence(List(prepareMove, move, afterMove))
        }

        val moveArg = {
          val prepareMove = TMManip.workOn(SimpleOps.replaceSymbol(Alph(hash), Blank, next), 0, 2)
          val move = MultiTapeOps.moveDown2(valuesymbols, next)
          val afterMove = TMManip.workOn(SimpleOps.replaceSymbol(Blank, Alph(hash), next), 0, 2)

          ControlFlow.sequence(List(prepareMove, move, afterMove))
        }
        
        val findHash = TMManip.workOn(SimpleOps.findSymbol(namesymbols + comma, hash, next), 0, 2)
        val findRevHash = TMManip.workOn(SimpleOps.findSymbolRev(namesymbols + comma, hash, next), 0, 2)

        // remove hashes
        val finish = TMManip.workOn(ControlFlow.sequence(List(
          SimpleOps.findSymbol(Set(), hash, next),
          SimpleOps.replaceSymbol(Alph(hash), Blank, next),
          SimpleOps.findSymbolRev(Set(), hash, next),
          SimpleOps.replaceSymbol(Alph(hash), Blank, next)
        )), 0, 2)

        ControlFlow.sequence(List(moveParam, findHash, moveArg, findRevHash, finish))
      }

      val loopOnComma = {
        val moveParam = {
          val prepareMove = TMManip.workOn(ControlFlow.sequence(List(
            SimpleOps.replaceSymbol(Alph(comma), Blank, next),
            SimpleOps.findSymbolRev(namesymbols, hash, next),
            SimpleOps.replaceSymbol(Alph(hash), Blank, next)
          )), 0, 2)
          val move = MultiTapeOps.moveDown2(namesymbols, next)
          val afterMove = TMManip.workOn(SimpleOps.replaceSymbol(Blank, Alph(hash), next), 0, 2)
          ControlFlow.sequence(List(prepareMove, move, afterMove))
        }

        val moveArg = {
          val prepareMove = TMManip.workOn(SimpleOps.replaceSymbol(Alph(hash), Blank, next), 0, 2)
          val move = MultiTapeOps.moveDown2(valuesymbols, next)
          val afterMove = TMManip.workOn(SimpleOps.replaceSymbol(Blank, Alph(hash), next), 0, 2)

          ControlFlow.sequence(List(prepareMove, move, afterMove))
        }
        
        val findHash = TMManip.workOn(SimpleOps.findSymbol(namesymbols + comma, hash, next), 0, 2)
        val findRevHash = TMManip.workOn(SimpleOps.findSymbolRev(namesymbols + comma, hash, next), 0, 2)
        ControlFlow.sequence(List(moveParam, findHash, moveArg, findRevHash))
      }

      val List(q0, qprepare, qfindeither, qlooponblank, qlooponcomma, qaftercommaloop, qafterblankloop) = initStates(7, next)
      val findeither = TMManip.workOn(
        findOptions(namesymbols + hash, List(
          Alph(comma) -> qlooponcomma,
          Blank -> qlooponblank,
        ), next), 0, 2) // , -> accept, # -> reject
      
      val parent = MultiMachine[Int, A](q0, Map(
        (qafterblankloop, List(Alph(hash), Blank)) -> (qfindeither, List(Alph(hash), Blank), List(Stay, Stay)),
        (qaftercommaloop, List(Alph(hash), Blank)) -> (qfindeither, List(Alph(hash), Blank), List(Stay, Stay)),
        (q0, List(Blank, Blank)) -> (qprepare, List(Blank, Blank), List(Stay, Stay),
      )))

      import ControlFlow._
      insertMachines(parent, List(
        prepare -> MachineConnection(qprepare, qfindeither, Reject),
        findeither -> MachineConnection(qfindeither, Accept, Reject),
        loopOnComma -> MachineConnection(qlooponcomma, qaftercommaloop, Reject),
        loopOnBlank -> MachineConnection(qlooponblank, qafterblankloop, Reject)
      ))
    }
  }

  /**
   * input:  [Δ]
   *          ^
   *         [#x1Δv1Δ...ΔxnΔvnΔ]
   *                          ^
   * output: [Δc;x1,v1,...,xn,vn;p1,p2,...,pkΔ] (where `params` = p1, ..., pk, c = `code`)
   *          ^
   *         [#x1Δv1Δ...ΔxnΔvnΔ]
   *                          ^
   */
  def functionValue[A](params: List[List[A]], code: List[A], symbols: Set[A], hash: A, comma: A, semicolon: A, next: () => Int): MultiMachine[Int, A] = {
    require(params.flatten.forall(symbols.contains(_)))

    val codesymbols = code.toSet
    val wrcode = TMManip.workOn(ControlFlow.sequence(List(
      SimpleOps.write(code, next),
      SimpleOps.nextBlank(codesymbols, next)
    )), 0, 2)
    val wrps = TMManip.workOn(Impl.writeParams(params, comma, next), 0, 2)
    val cpenv = Impl.copyEnv(symbols, comma, hash, next)
    val wrsemicolons = TMManip.workOn(ControlFlow.sequence(List(
      SimpleOps.replaceSymbol(Blank, Alph(semicolon), next),
      SimpleOps.prevBlank(symbols + semicolon + comma, next),
      SimpleOps.replaceSymbol(Blank, Alph(semicolon), next),
      SimpleOps.prevBlank(codesymbols + semicolon + comma, next)
    )), 0, 2)

    ControlFlow.sequence(List(wrcode, cpenv, wrps, wrsemicolons))
  }

  case class Branch[A](code: List[A], state: NonHalt[Int])
  def areAmbiguous[A](branches: List[Branch[A]]): Boolean = {
    var set: Set[List[A]] = Set()
    branches.exists {
      case Branch(code, _) => // TODO: cleaner
        val b = set.contains(code)
        set = (1 to code.length).foldLeft(set)((set, i) => set + code.take(i))
        b
    }
  }

  /**
   * input:  ΔcΔ (where c is a code in `branches`)
   *           ^
   * output: ΔcΔ (in the state corresponding to c in `branches`)
   *           ^
   */
  def brancher[A](branches: List[Branch[A]], next: () => Int): TuringMachine[Int, A] = {
    require(branches.forall(_.code.nonEmpty))
    require(!areAmbiguous(branches))

    val symbols = branches.map(_.code).flatten.toSet

    // map from every partial word to the 'next symbol'
    // so for a code x1x2x3...xixi+i...xn, we have
    // x1x2x3...xi -> xi+1 for every 0 <= i <= n-1
    val connections: Map[List[A], Set[A]] = branches.map {
      case Branch(code, state) => 
        (0 to code.length - 1).map(i => code.take(i) -> code(i))
    }.flatten.foldLeft(Map()) {
      case (map, (str, symb)) => 
        val old = map.get(str).getOrElse(Set())
        map.updated(str, old + symb)
    }

    // create the branching states, and map each destination word to the
    //  corresponding state
    val codeToState: Map[List[A], NonHalt[Int]] = 
      branches.map { case Branch(code, state) => (code, state) }.toMap ++ // destinations
      connections.map((str, _) => (str, NonHalt(next())))                 // branching nodes
  
    assert(connections.keySet.exists(s => s.isEmpty)) // empty word should have a node
    val qemptyword = codeToState(Nil) // before reading: code is 

    val List(q0, q1, qpb) = initStates(3, next)
    val pb = SimpleOps.prevBlank(symbols, next)

    val transitions = connections.map {
      case (str, ss) => 
        val qf = codeToState(str)
        ss.map(s => {
          val qt = codeToState(str.appended(s))
          (qf, Alph(s)) -> (qt, Alph(s), Right)
        })
    }.flatten
    
    val parent = TuringMachine(q0, Map(
      (q0, Blank) -> (qpb, Blank, Stay),
      (q1, Blank) -> (qemptyword, Blank, Right)
    ) ++ transitions)
    
    ControlFlow.insertMachine(parent, pb, qpb, q1, Reject)
  }

  /**
   * input:  [Δc;x1,v1,...,xn,vn;p1,...,pkΔa1Δ...ΔakΔ]
   *          ^
   *         [#y1Δu1Δ...ΔymΔumΔ]
   *                          ^
   *         [Δ]
   *          ^
   * output: [Δ]
   *          ^
   *         [#y1Δu1Δ...ΔymΔumΔx1Δv1Δ...ΔxnΔvnΔp1Δa1Δ...ΔpkΔakΔ]
   *                                                          ^
   *         [ΔcΔ]
   *            ^
   */
  def prepareFunctionCall[A](namesymbols: Set[A], valuesymbols: Set[A], codesymbols: Set[A], 
                             comma: A, semicolon: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
    val loadcode = TMManip.workOn(
      MultiTapeOps.moveDownUntil(codesymbols, semicolon, next),
      List(0, 2), 3)
    val loadenv = ControlFlow.sequence(
      TMManip.workOn(MultiTapeOps.moveDownUntil(namesymbols ++ valuesymbols + comma, semicolon, next), List(0, 1), 3),
      TMManip.workOn(Impl.replaceWithBlankBehindUntil(namesymbols ++ valuesymbols, hash, comma, semicolon, next), 1, 3)
    )

    ControlFlow.sequence(List(
      loadcode, loadenv
    ))
  }
}
