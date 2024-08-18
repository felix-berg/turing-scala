package turing.machines

object Functions {
  import turing.TuringMachine._
  import turing.ControlFlow

  object Impl {
    /**
     * input:  Δ
     *         ^
     * output: Δp1,p2,...,pkΔ
     *                      ^
     */
    def writeParams[A](params: List[List[A]], comma: A, next: () => Int): TuringMachine[Int, A] = 
      if (params.isEmpty) return SimpleOps.accept(NonHalt(next()), Set()) else {
        val set = params.toSet.flatten
        val writes = params.map(param => List(
          SimpleOps.write[A](param, next), 
          SimpleOps.nextBlank[A](set, next)
        )).flatten.dropRight(1)

        val goBack = (1 to params.size - 1).map(_ => List(
          SimpleOps.replaceSymbols(Blank, Alph(comma), next),
          SimpleOps.prevBlank(set + comma, next)
        )).flatten.toList

        ControlFlow.sequence(writes ++ goBack)
      }
  }

  /**
   * input:  [Δ]
   *          ^
   *         [Δx1Δv1Δ...ΔxnΔvnΔ]
   *                          ^
   * output: [Δp1,p2,...,pk;x1,v1,...,xn,vn;cΔ] (where `params` = p1, ..., pk, c = `code`)
   *          ^
   *         [Δx1Δv1Δ...ΔxnΔvnΔ]
   *                          ^
   */
  def functionValue[A](params: List[List[A]], code: List[A], sp1: A, sp2: A, next: () => Int): MultiMachine[Int, A] = {
    ???
  }
}
