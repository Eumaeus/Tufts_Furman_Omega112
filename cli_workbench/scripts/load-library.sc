import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

// Config Stuff
val cexfile: String = "/Users/cblackwell/Dropbox/CITE/digital-libraries/fuCiteDX/tokenized_exemplars/iliad_odyssey_all.cex"

// Load our CITE Library
val lib: CiteLibrary = loadLibrary(cexfile)

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

// Find out what texts are in this library
// … a Set is a Collection where each value is unique, btw
val versions: Set[CtsUrn] = tr.catalog.versions
// Let's get a URN to work with
val iliad: CtsUrn = versions.head // will throw an error if there are no versions!

// We can use Scala-strings to make a new URN
val aBook1Urn: CtsUrn = {
	val uString: String  = s"${iliad}1"
	CtsUrn(uString)
}

// Or we can use the API
val bBook1Urn: CtsUrn = iliad.addPassage("1")

// Grab a sub-corpus
val bookOne: Corpus = corp ~~ bBook1Urn



