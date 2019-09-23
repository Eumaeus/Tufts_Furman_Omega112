package edu.furman.csc270

import scala.io.Source
import edu.holycross.shot.cite._

/** 

A template for an "app" that you can compile, and for which SBT will give
decent feeback.

*/

object MyApp {
 
  /** Our main function where the action happens */
  def main(args: Array[String]) {
    try {
      val punctuation: String = """[“”()\[\]·-…⸁.,; "?·!–—⸂⸃]"""
      val alphabet: String = """[A-Za-z]"""   

      if (args.size > 0) {
        println(s"argument: ${args(0)}")
      } else {
        println(s"No argument passed.")
      }
    } catch {
      case e:Exception => s"Exception: ${e}"
    }

    def loadFile(fp:String):Vector[String] = {
        Source.fromFile(fp).getLines.toVector
    }

  }
}
