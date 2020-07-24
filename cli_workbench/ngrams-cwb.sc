import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

// Config Stuff
val cexfile: String = "../Master-CEX/iliad_for_ngrams.cex"

// Load our CITE Library
lazy val lib: CiteLibrary = loadLibrary(cexfile)

lazy val tr: TextRepository = lib.textRepository.get

lazy val corp: Corpus = tr.corpus

val iliadLemUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.lemmatized:")
val iliadUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.allen:")
val odysseyUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg002.murray:")

// Get NGrams

/* User-editable values */
val myN: Int = 6 // how many words in the pattern
val myThreshold: Int = 10 // occurs more than this many times
val fileName: String = "ngram-output.md" // save in `texts/` under this name

/* end User-editable values */

val myNGs: StringHistogram = corp.ngramHisto(myN,myThreshold)

case class NGReport( count: Int, string: String, urns: Vector[CtsUrn]) {
	override def toString: String = {
		s"""${count}\t${string}\n\n${urns.mkString("\n")}"""
	}
}


lazy val myReports: Vector[NGReport] = myNGs.histogram.map( ng => {
	val count: Int = ng.count
	val string: String = ng.s
	val urns: Vector[CtsUrn] = corp.urnsForNGram(string)
	NGReport(count, string, urns)
})

val report: String = myReports.map( _.toString ).mkString("\n-------\n")

saveString(report, fileName = fileName)

