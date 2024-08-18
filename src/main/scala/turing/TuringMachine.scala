package turing

object TuringMachine {
  sealed abstract class Direction
  case object Left extends Direction
  case object Right extends Direction
  case object Stay extends Direction

  sealed abstract class TapeAlph[+T]
  case class Alph[T](a: T) extends TapeAlph[T]
  case object Blank extends TapeAlph[Nothing]

  sealed abstract class State[+Q]
  case class NonHalt[Q](q: Q) extends State[Q]
  case object Accept extends State[Nothing]
  case object Reject extends State[Nothing]

  type TransTable[Q, A] = Map[(NonHalt[Q], TapeAlph[A]), (State[Q], TapeAlph[A], Direction)]
  case class TuringMachine[Q, A](init: NonHalt[Q], transitions: TransTable[Q, A])

  type MultiTable[Q, A] = Map[(NonHalt[Q], List[TapeAlph[A]]), (State[Q], List[TapeAlph[A]], List[Direction])]
  case class MultiMachine[Q, A](init: NonHalt[Q], transitions: MultiTable[Q, A])

  case class Configuration[Q, A](left: List[TapeAlph[A]], state: State[Q], right: List[TapeAlph[A]]) {
    override def toString: String =
      (state match {
        case NonHalt(q) => q.toString
        case Accept => "ha"
        case Reject => "hr"
      }).padTo(4, ' ') +
      ": (" + left.reverse.map {
        case Blank => "Δ"
        case Alph(x) => x.toString
      }.mkString("") + ", " + right.map {
        case Blank => "Δ"
        case Alph(x) => x.toString
      }.mkString("") + ")"

    def collapse: Configuration[Q, A] = Configuration(left, state, collapseTape(right))
  }

  def collapseTape[A](tape: List[TapeAlph[A]]): List[TapeAlph[A]] = tape match {
    case Nil => Nil
    case x :: xs => collapseTape(xs) match {
      case Nil if x == Blank => Nil
      case t => x :: t
    }
  }

  case class MultiConfig[Q, A](lefts: List[List[TapeAlph[A]]], state: State[Q], rights: List[List[TapeAlph[A]]]) {
    override def toString: String =
      (state match {
        case NonHalt(q) => q.toString
        case Accept => "ha"
        case Reject => "hr"
      }) + ":\n" + lefts.zip(rights).map {
        case (l, r) => "(" + l.reverse.map {
          case Blank => "Δ"
          case Alph(x) => x.toString
        }.mkString("") + ", " + r.map {
          case Blank => "Δ"
          case Alph(x) => x.toString
        }.mkString("") + ")\n"
      }.mkString("")

    def collapse: MultiConfig[Q, A] = MultiConfig(lefts, state, rights.map(collapseTape))
  }

  def getCurrentSymbol[A](conf: Configuration[?, A]): TapeAlph[A] = conf.right match {
    case x :: xs => x
    case Nil => Blank
  }

  def moveTape[A](left: List[TapeAlph[A]], right: List[TapeAlph[A]], dir: Direction, symb: TapeAlph[A]): (List[TapeAlph[A]], List[TapeAlph[A]]) =
    (right, dir) match {
      case (Nil, Stay) => (left, symb :: Nil)
      case (Nil, Right) => (symb :: left, Nil)

      case (x :: xs, Stay) => (left, symb :: xs)
      case (x :: xs, Right) => (symb :: left, xs)

      case (_, Left) => left match {
        case Nil => ???; (Nil, Nil) // something better
        case y :: ys => right match {
          case Nil => (ys, y :: symb :: Nil)
          case x :: xs => (ys, y :: symb :: xs)
        }
      }
    }

  def step[Q, A](machine: TuringMachine[Q, A], conf: Configuration[Q, A]): Configuration[Q, A] = conf.state match {
    case Accept | Reject => conf
    case NonHalt(q1) => machine.transitions.get((NonHalt(q1), getCurrentSymbol(conf))) match {
      case None => Configuration(conf.left, Reject, conf.right)
      case Some((q2, symb, dir)) =>
        val (l, r) = moveTape(conf.left, conf.right, dir, symb)
        Configuration(l, q2, r)
    }
  }

  def step[Q, A](machine: MultiMachine[Q, A], conf: MultiConfig[Q, A]): MultiConfig[Q, A] = conf.state match {
    case Accept | Reject => conf
    case NonHalt(q1) =>
      val ss = conf.rights.map {
        case x :: xs => x
        case Nil => Blank
      }

      machine.transitions.get((NonHalt(q1), ss)) match {
        case None => MultiConfig(conf.lefts, Reject, conf.rights)
        case Some((q2, symbols, dirs)) =>
          val (leftss, rightss) = conf.lefts.zip(conf.rights).zip(symbols).zip(dirs).map {
            case (((l, r), s), d) => moveTape(l, r, d, s)
          }.unzip

          MultiConfig(leftss, q2, rightss)
      }
  }
}
