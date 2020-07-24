import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

// Config Stuff
val cexfile: String = "../Master-CEX/iliad_for_ngrams.cex"

// Load our CITE Library
lazy val lib: CiteLibrary = loadLibrary(cexfile)

lazy val tr: TextRepository = lib.textRepository.get

lazy val corp: Corpus = tr.corpus

