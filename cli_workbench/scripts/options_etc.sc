import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

// Config Stuff
val cexfile: String = "texts/jane_austen.cex"

// Load our CITE Library
lazy val lib: CiteLibrary = loadLibrary(cexfile)

lazy val tr: TextRepository = lib.textRepository.get

lazy val corp: Corpus = tr.corpus

// Find out what texts are in this library
// … a Set is a Collection where each value is unique, btw
lazy val versions: Set[CtsUrn] = tr.catalog.versions
// Let's get a URN to work with
lazy val iliad: CtsUrn = versions.head // will throw an error if there are no versions!

val hasEx:CtsUrn = CtsUrn(s"urn:cts:greekLit:tlg0012.tlg001.msA.tok:")
val noEx:CtsUrn = CtsUrn(s"urn:cts:greekLit:tlg0012.tlg001.msA:")

def exName(u: CtsUrn):String = {
		u.exemplarOption match {
			case Some(e) => s""""${e}""""  
			case None => "No exemplar!"
		}	
}

def exName2( u: CtsUrn):String = {
	u.exemplarOption.getOrElse("No exemplar!")
}

