import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

// Config Stuff
val cexfile: String = "texts/hmt_test.cex"

// Load our CITE Library
val lib: CiteLibrary = loadLibrary(cexfile)

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

// Find out what texts are in this library
// … a Set is a Collection where each value is unique, btw
val versions: Set[CtsUrn] = tr.catalog.versions
// Let's get a URN to work with
val iliad: CtsUrn = versions.head // will throw an error if there are no versions!

// Or we can use the API
val book1Urn: CtsUrn = iliad.addPassage("1")
val line1Urn: CtsUrn = iliad.addPassage("1.1")

// Grab a sub-corpus
val bookOne: Corpus = corp ~~ book1Urn

// Find an image-roi
val cr: CiteCollectionRepository = lib.collectionRepository.get

val dseUrn: Cite2Urn = Cite2Urn("urn:cite2:hmt:va_dse.v1:")



