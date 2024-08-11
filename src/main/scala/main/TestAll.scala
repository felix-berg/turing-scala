package turing

import scala.annotation.tailrec
import TuringMachine._
import TMSimulate._
import scala.util.Random

object TestAll {
  import turing.machines._

  def main(args: Array[String]): Unit = {

    val standardMethods = Set(
      "equals",
      "toString",
      "hashCode",
      "getClass",
      "notify",
      "notifyAll",
      "wait",
      "main"
    )

    val packages = List(
      // "turing.tests",
      "miniscala.tests"
    )

    val dirs = packages
        .map(p => p.replace(".", "/"))
        .map(p => s"src/main/scala/$p")
        .map(new java.io.File(_))
    

    assert(dirs.forall(_.isDirectory))

    val ignoredClasses = List("turing.tests.TestUtils")

    val testclasses = packages.zip(dirs)
      .map((p, d) => d.listFiles
        .map(file => file.getName.split(".scala")(0))
        .map(filename => s"$p.$filename")
        .filter(classname => !ignoredClasses.contains(classname))
        .map(classname => Class.forName(classname))
      ).flatten

    testclasses.foreach(klass => {
      println(s"${klass.getName}")
      klass.getMethods
        .filter(f => !standardMethods.contains(f.getName))
        .filter(f => f.getParameterTypes.isEmpty)
        .foreach(f => {
          println(" - " + f.getName)
          f.invoke(this)
        })
    })
  }
}
