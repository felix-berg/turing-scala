package turing.tests

object TestUnaryOps {
  import turing.TuringMachine._
  import turing.machines._

  def dumbAdd(): Unit = {
    val next = {
      var n = -1
      () => {
        n += 1; n
      }
    }
    
    val machine = Unary.add('1', next)

    for (i <- 0 to 8) {
      val max = Math.pow(2, i).toInt
      val m = scala.util.Random.nextInt(max)
      val n = scala.util.Random.nextInt(max)
      
      val sn = (0 until n).map(_ => Alph('1')).toList
      val sm = (0 until m).map(_ => Alph('1')).toList

      val input = Blank :: sn ++ (Blank :: sm)
      val bconf = Configuration(Nil, machine.init, input)
      val conf = turing.TMSimulate.runConfiguration(machine, bconf).collapse

      val expected = collapseTape(Blank :: (0 until (m + n)).map(_ => Alph('1')).toList)
      assert(conf.state == Accept, s"non-accepted state")
      assert(conf.left == Nil, s"left is non-empty")
      assert(conf.right == expected, s"${conf.right} != $expected (m = $m, n = $n, input = $input)")
    }
  }

  def dumbSub(): Unit = {
    val next = {
      var n = -1
      () => {
        n += 1; n
      }
    }

    
    val machine = Unary.sub('1', '#', next)

    for (i <- 0 to 8) {
      val max = Math.pow(2, i).toInt
      val n = scala.util.Random.nextInt(max)
      val m = scala.util.Random.nextInt(max - n) + n
      assert(m >= n, s"bug in test,bug in test,  $m < $n")
      
      val sn = (0 until n).map(_ => Alph('1')).toList
      val sm = (0 until m).map(_ => Alph('1')).toList

      val input = Blank :: sm ++ (Blank :: sn)
      val bconf = Configuration(Nil, machine.init, input)
      val conf = turing.TMSimulate.runConfiguration(machine, bconf).collapse

      val expected = collapseTape(Blank :: (0 until (m - n)).map(_ => Alph('1')).toList)
      assert(conf.state == Accept, s"non-accepted state")
      assert(conf.left == Nil, s"left is non-empty")
      assert(conf.right == expected, s"${conf.right} != $expected (m = $m, n = $n, input = $input)")
    }
  }
}
