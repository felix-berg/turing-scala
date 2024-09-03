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
     *         [#Δx1Δv1Δ...ΔxnΔvnΔ]
     *                           ^
     * output: [Δx1,v1,...,xn,vnΔ] (where `params` = p1, ..., pk, c = `code`)
     *                          ^
     *         [#Δx1Δv1Δ...ΔxnΔvnΔ]
     *                           ^
     */
    def copyEnv[A](symbols: Set[A], comma: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
      val (cm, hs) = (Alph(comma), Alph(hash))

      val prepareEnv = TMManip.workOn({
        val List(q0, q1, q2, q3, qpb) = initStates(5, next)
        val pb = SimpleOps.prevBlankWithStop(symbols + comma, hash, next)
        val parent = TuringMachine(q0, Map(
          (q0, Blank) -> (qpb, Blank, Stay),
          (q1, Blank) -> (qpb, cm, Stay),
          (q2, hs) -> (q3, hs, Right),
          (q3, cm) -> (Accept, Blank, Stay), // remove first comma 
          (q3, Blank) -> (Accept, Blank, Stay)
        ))
        ControlFlow.insertMachine(parent, pb, qpb, q1, q2)
      }, 1, 2)
  
      val copyToMain = TMManip.workOn(
        MultiTapeOps.copyDown(symbols + comma, next),
        List(1, 0), 2 // invert direction
      )
      val cleanEnvIfNonEmpty = TMManip.workOn({
        val cleanEnv = {
          val List(q0, q1, qnb) = initStates(3, next)
          val nb = SimpleOps.nextBlankWithStop(symbols + hash, comma, next)
          val parent = TuringMachine(q0, Map(
            (q0, Blank) -> (qnb, Blank, Stay),
            (q1, cm) -> (qnb, Blank, Stay)
          ))

          ControlFlow.insertMachine(parent, nb, qnb, Accept, q1)
        }

        ControlFlow.ifThenElse(SimpleOps.isEmpty(symbols, next), 
          SimpleOps.transitionToState(Accept, next),
          cleanEnv)
      }, 1, 2)

      val forward = TMManip.workOn(
          SimpleOps.nextBlank(symbols + comma, next),
        0, 2)

      ControlFlow.sequence(List(prepareEnv, copyToMain, cleanEnvIfNonEmpty, forward))
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
     * if input:    [#pi,...,pkΔ^n#aiΔ...Δak]
     *               ^
     *              [Δ]
     *               ^
     * then output: [Δ^p#pi+1,...,pkΔ^m#ai+1Δ...Δak]
     *                  ^
     *              [ΔpiΔaiΔ]
     *                     ^
     * if input:    [#Δ^n#] for some n >= 0
     *               ^
     *              [Δ]
     *               ^
     * then output: [Δ] (and reject) 
     *               ^
     *              [Δ]
     *               ^
     *
     */
    def loadParamsIterate[A](namesymbols: Set[A], valuesymbols: Set[A], comma: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
      // require(namesymbols.intersect(valuesymbols).isEmpty)
      val List(q0, q1, qempty, qnonempty) = initStates(4, next)
      
      val whenEmpty = TMManip.workOn({
        ControlFlow.sequence(List(
          SimpleOps.replaceSymbol(Alph(hash), Blank, next)
        ))
      }, 0, 2)

      val whenNonEmpty = {
        val beforeMoveParam = TMManip.workOn(SimpleOps.replaceSymbol(Alph(hash), Blank, next), 0, 2)
        val moveParam = MultiTapeOps.moveDownUntilSetOrBlank(namesymbols, Set(comma, hash), next)
        val afterMoveParam = TMManip.workOn(ControlFlow.sequence(List(
          SimpleOps.moveLeft(namesymbols + comma, next),
          SimpleOps.replaceSymbol(Blank, Alph(hash), next),
          SimpleOps.findAnySymbol(namesymbols + comma + hash, valuesymbols, next),
          SimpleOps.moveLeft(valuesymbols, next)
        )), 0, 2)
        val moveValue = MultiTapeOps.moveDown2(valuesymbols, next)
        val afterMoveValue = TMManip.workOn(ControlFlow.sequence(List(
          SimpleOps.findSymbolRev(valuesymbols ++ namesymbols + comma, hash, next),
          SimpleOps.replaceSymbol(Alph(hash), Blank, next),
          SimpleOps.moveRight(Set(), next),
          SimpleOps.replaceSymbol(Blank, Alph(hash), next)
        )), 0, 2)
        
        ControlFlow.sequence(List(
          beforeMoveParam, moveParam, afterMoveParam,
          moveValue, afterMoveValue
        ))
      }
      
      val par = MultiMachine(q0, Map( // different from drawing
        (q0, List(Alph(hash), Blank)) -> (q1, List(Alph(hash), Blank), List(Right, Stay)),
        (q1, List(Blank, Blank)) -> (qempty, List(Blank, Blank), List(Left, Stay)), 
        ) ++ namesymbols.map(s => 
          (q1, List(Alph(s), Blank)) -> (qnonempty, List(Alph(s), Blank), List(Left, Stay))
        ))

      ControlFlow.insertMachines(par, List(
        whenEmpty -> ControlFlow.MachineConnection(qempty, Reject, Reject),
        whenNonEmpty -> ControlFlow.MachineConnection(qnonempty, Accept, Reject)
      ))
    }

    /**
     * input:  [Δp1,p2,...,pkΔa1Δ...ΔakΔ]
     *          ^
     *         [Δ]
     *          ^
     * output: [ΔΔ^nΔ] (where n = |p1| + ... + |pk|)
     *              ^
     *         [Δp1Δa1Δ...ΔpkΔakΔ]
     *                          ^
     */
    def loadParamsToEnv[A](namesymbols: Set[A], valuesymbols: Set[A], comma: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
      val loop = {
        val List(ql, qloadparamsiterate) = initStates(2, next)
        val loadparamsiterate = loadParamsIterate(namesymbols, valuesymbols, comma, hash, next)
        val par = MultiMachine(ql, Map(
          (ql, List(Alph(hash), Blank)) -> (qloadparamsiterate, List(Alph(hash), Blank), List(Stay, Stay))
        ))
        ControlFlow.insertMachine(par, loadparamsiterate, qloadparamsiterate, ql, Accept)
      }

      val setup = TMManip.workOn(ControlFlow.sequence(List(
        SimpleOps.replaceSymbol(Blank, Alph(hash), next),
      )), 0, 2)

      ControlFlow.sequence(setup, loop)
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
   * output: Δ (in the state corresponding to c in `branches`)
   *         ^
   */
  def brancher[A](branches: List[Branch[A]], next: () => Int): TuringMachine[Int, A] = {
    require(branches.forall(_.code.nonEmpty))
    require(!areAmbiguous(branches))
    require(branches.nonEmpty)
    
    val symbols = branches.map(_.code).flatten.toSet

    // map from every partial word to the 'next symbol'
    // so for a code x1x2x3...xixi+i...xn, we have
    // x1x2x3...xi -> xi+1 for every 0 <= i <= n-1
    val connections: Map[List[A], Set[A]] = branches.map {
      case Branch(code, state) => 
        (0 to code.length - 1).map(i => code.takeRight(i) -> code(code.size - 1 - i))
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

    val List(q0) = initStates(1, next)

    val transitions = connections.map {
      case (str, ss) => 
        val qf = codeToState(str)
        ss.map(s => {
          val qt = codeToState(s :: str)
          (qf, Alph(s)) -> (qt, Blank, Left)
        })
    }.flatten
    

    TuringMachine(q0, Map(
      (q0, Blank) -> (qemptyword, Blank, Left)
    ) ++ transitions)
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
   *         [#y1Δu1Δ...ΔymΔumΔ#x1Δv1Δ...ΔxnΔvnΔp1Δa1Δ...ΔpkΔakΔ]
   *                                                          ^
   *         [ΔcΔ]
   *            ^
   */
  def prepareFunctionCall[A](namesymbols: Set[A], valuesymbols: Set[A], codesymbols: Set[A], comma: A, semicolon: A, hash: A, next: () => Int): MultiMachine[Int, A] = {
    val movecode = TMManip.workOn(MultiTapeOps.moveDownUntilAndMarkSourceBeginning(codesymbols, semicolon, hash, next), List(0, 2), 3)
    val loadenv = {
      val go = ControlFlow.sequence(List(
        TMManip.workOn(SimpleOps.moveRight(Set(hash), next), 1, 3),
        TMManip.workOn(MultiTapeOps.moveDownUntilAndMarkDestinationBeginning(namesymbols ++ valuesymbols + comma, semicolon, hash, next), List(0, 1), 3),
        TMManip.workOn(Impl.replaceWithBlankBehindUntil(namesymbols ++ valuesymbols, hash, comma, semicolon, next), 1, 3)
      ))

      val List(q0, q1, q2, qgo) = initStates(4, next)
      val par = MultiMachine(q0, Map(
        // check if empty
        (q0, List(Blank, Blank, Blank)) -> (q1, List(Blank, Blank, Blank), List(Right, Stay, Stay)),
        // if so, only place semicolon then stop
        (q1, List(Alph(semicolon), Blank, Blank)) -> (q2, List(Blank, Blank, Blank), List(Stay, Right, Stay)),
        (q2, List(Blank, Blank, Blank)) -> (Accept, List(Blank, Alph(hash), Blank), List(Stay, Right, Stay))
      ) ++ namesymbols.map(c => 
        (q1, List(Alph(c), Blank, Blank)) -> (qgo, List(Alph(c), Blank, Blank), List(Left, Stay, Stay))
      ))

      ControlFlow.insertMachine(par, go, qgo, Accept, Reject)
    }

    val loadparams = TMManip.workOn(Impl.loadParamsToEnv(namesymbols, valuesymbols, comma, hash, next), List(0, 1), 3)
    val cleanhash = TMManip.workOn(ControlFlow.sequence(
      SimpleOps.findSymbolRev(Set(), hash, next),
      SimpleOps.replaceSymbol(Alph(hash), Blank, next)
    ), 0, 3)

    ControlFlow.sequence(List(movecode, loadenv, loadparams, cleanhash))
  }

  /*
   * input:  [Δvalue...]
   *          ^
   *         [Δ#x1Δv1Δ...ΔxnΔvnΔ]
   *                          ^
  *         [Δ]
   *          ^
   * output: [Δvalue...]
   *          ^
   *         [Δ]
   *          ^
   *         [Δ]
   *          ^
   */
  def cleanAfterFunctionCall[A](namesymbols: Set[A], valuesymbols: Set[A], codesymbols: Set[A], hash: A, next: () => Int): MultiMachine[Int, A] = {
    TMManip.workOn(
      ControlFlow.sequence(
        SimpleOps.deleteUntilSymbolRev(namesymbols ++ valuesymbols, hash, next), 
        SimpleOps.moveLeft(Set(), next)
      ),
    1, 3)
  }
}
