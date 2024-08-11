package turing

object TMSimulate {
  import scala.annotation.tailrec
  import TuringMachine._

  @tailrec
  def runConfiguration[Q, A](machine: TuringMachine[Q, A], conf: Configuration[Q, A]): Configuration[Q, A] =
    if (conf.state == Accept || conf.state == Reject)
      conf
    else
      runConfiguration(machine, step(machine, conf))

  @tailrec
  def runConfiguration[Q, A](machine: MultiMachine[Q, A], conf: MultiConfig[Q, A]): MultiConfig[Q, A] =
    if (conf.state == Accept || conf.state == Reject) conf
    else runConfiguration(machine, step(machine, conf))

  def run[Q, A](machine: TuringMachine[Q, A], input: List[TapeAlph[A]]): Configuration[Q, A] =
    runConfiguration(machine, Configuration[Q, A](Nil, machine.init, Blank :: input))

  @tailrec
  def printRunConfiguration[Q, A](m: TuringMachine[Q, A], conf: Configuration[Q, A], delayms: Int): Configuration[Q, A] = {
    println(conf)
    Thread.sleep(delayms)

    if (conf.state == Accept || conf.state == Reject) conf
    else printRunConfiguration(m, step(m, conf), delayms)
  }

  @tailrec
  def printRunConfiguration[Q, A](m: MultiMachine[Q, A], conf: MultiConfig[Q, A], delayms: Int): MultiConfig[Q, A] = {
    println(conf)
    Thread.sleep(delayms)

    if (conf.state == Accept || conf.state == Reject) conf
    else printRunConfiguration(m, step(m, conf), delayms)
  }

  def printRun[Q, A](m: TuringMachine[Q, A], input: List[TapeAlph[A]], delayms: Int): Configuration[Q, A] =
    printRunConfiguration(m, Configuration(Nil, m.init, Blank :: input), delayms)
}
