import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

def getHisto(c: Corpus): Vector[(String, Int)] = {
	// Map each passsage of the Corpus to a Vector of tokens
	val tokenVec: Vector[CitableNode] = {
		c.nodes.map( n => {
				val eu: CtsUrn = n.urn.dropPassage.addExemplar("tok")
				val l: String = n.text		
				val u: CtsUrn = n.urn
				val v: Vector[String] = splitWithSplitter(l)
				val tokVec: Vector[CitableNode] = v.zipWithIndex.map( t => {
					val index: Int = t._2
					val newPassage: String = s"${u.passageComponent}.${index}"
					val newUrn: CtsUrn = eu.dropPassage.addPassage(newPassage)
					CitableNode(urn = newUrn, text = t._1)
				})
				tokVec
		}).flatten
	}
	// Let's filter out punctuation, since we have it defined anyway!
	val noPuncVec: Vector[CitableNode] = {
		tokenVec.filter( t => {
			t.text.replaceAll(punctuation,"").size > 0
		})
	}

	// Make a Corpus out of that Vector[CitableNode]
	val tokenCorpus: Corpus = Corpus(noPuncVec)

	// Make a histogram:
	val tokenHisto: Vector[(String, Int)] = {
		// we just want the text, not the urn…
		val justText: Vector[String] = tokenCorpus.nodes.map(_.text)
		// map it…
		val tokMap: Map[String, Vector[String]] = justText.groupBy( m => m)
		// replace the Vector[String] with the _size_ of the Vector[String]
		tokMap.toVector.map( t => {
			(t._1, t._2.size)
		}).sortBy(_._2).reverse
	}
	tokenHisto
}

// Config Stuff
val cexfile: String = "texts/hmt_test.cex"

// Load our CITE Library
val lib: CiteLibrary = loadLibrary(cexfile)

val tr: TextRepository = lib.textRepository.get

val corp: Corpus = tr.corpus

val iliadUrn: CtsUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001:")
val scholiaUrn: CtsUrn = CtsUrn("urn:cts:greekLit:tlg5026:")
val msAUrn: CtsUrn = iliadUrn.addVersion("msA")


/* Today's assignment will be to do a histogram
   of word-frequency in the Iliad.
*/

// Let's make a tokenized exemplar!
val iliadCorpus: Corpus = corp ~~ msAUrn
val scholiaCorpus: Corpus = corp ~~ scholiaUrn







