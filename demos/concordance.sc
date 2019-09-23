import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

:load utilities.sc

object concordance {

	// Values

	val fullStops:Vector[String] = "; ? . :".split(" ").toVector
	val halfStops:Vector[String] = ", ( )".split(" ").toVector
	val splitters: String = """[()\[\]·⸁.,; "?·!–—⸂⸃]"""

	def preProcess(text: String): String = {
		// Fix Mr. Ms. Mrs.
		val removePeriods =   Vector("Mr", "Mrs", "Dr", "etc", "Ms", "[ABCDEFGHJKLMNOPQRSTUVWXYZ]")
		val matcher = s"""(${removePeriods.mkString("|")})\\."""
		text.replaceAll(matcher,"$1")
	}

}


