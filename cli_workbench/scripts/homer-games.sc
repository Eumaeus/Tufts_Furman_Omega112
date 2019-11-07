import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

/* Make a histogram of citable passages.
	 (mostly intended for tokenized editions, but…)
*/

def makeHisto( c: Corpus): Vector[(String, Int)] = {
	val nodeStrings: Vector[String] = {
		c.nodes.map( n => {
			n.text.replaceAll(punctuation,"")
		}).filter(_.size > 0).filter(_ != "_")
	}
	val justText: Vector[String] = nodeStrings.map(_.toLowerCase)
	val tokMap: Map[String, Vector[String]] = justText.groupBy( m => m)
	tokMap.toVector.map( t => {
			(t._1, t._2.size)	
	}).sortBy(_._2).reverse
}

def ngramHisto(c: Corpus, n: Int, f: Int = 10): Vector[(String, Int)] = {
	// Get rid of empties, and punctuation
	val nodeStrings: Vector[String] = {
		c.nodes.map( n => {
			n.text.replaceAll(punctuation, "")
		}).filter( _.size > 0 ).filter( _ != "_")
	}
	val justText: Vector[String] = nodeStrings.map(_.toLowerCase)
	val ngVec: Vector[String] = justText.sliding(n, 1).toVector.map( vs => {
		vs.mkString(" ")
	})
	val tokMap: Map[String, Vector[String]] = ngVec.groupBy( m => m)
	tokMap.view.toVector.map( t => {
			(t._1, t._2.size)	
	}).filter(_._2 >= f).sortBy(_._2).reverse
}
// Config Stuff
val cexfile: String = "texts/iliad_odyssey_all.cex"

// Load our CITE Library
lazy val lib: CiteLibrary = loadLibrary(cexfile)

lazy val tr: TextRepository = lib.textRepository.get

lazy val corp: Corpus = tr.corpus

// Find out what texts are in this library
// … a Set is a Collection where each value is unique, btw
lazy val versions: Set[CtsUrn] = tr.catalog.versions

// Let's get some URNs to work with
val iU = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.allen:")
val iTU = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.allen.token:")
val iLU = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.allen.lemmatizedToken:")
val iLMU = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.allen.lemmatizedToken_merged:")

val oU = CtsUrn("urn:cts:greekLit:tlg0012.tlg002.murray:")
val oTU = CtsUrn("urn:cts:greekLit:tlg0012.tlg002.murray.token:")
val oLU = CtsUrn("urn:cts:greekLit:tlg0012.tlg002.murray.lemmatizedToken:")
val oLMU = CtsUrn("urn:cts:greekLit:tlg0012.tlg002.murray.lemmatizedToken_merged:")

lazy val iliadForms = corp >= iTU
lazy val iliadLemmata = corp >= iLU

lazy val formsHisto = makeHisto(iliadForms)
lazy val lemmataHisto = makeHisto(iliadLemmata)

/** Given a histogram, return the elements whose frequency sums to a given
  * percentage of the whole.
  * @param histo the Vector[(String, Int)] we are working with
  * @param targetPercent the Int that is the percentage we want to take from the histogram
  * @return a slice of the original histogram, whose members represent the requested percentage of the whole
*/
def takePercent( histo: Vector[(String, Int)], targetPercent: Int): Vector[(String, Int)] = {

	/** You can define functions _inside_ other functions!
	  * We define this so we can recurse until we get the right percentage.
	  * @param totalInstances the sum of the whole, which needs to be passed through each iteration, since the histo gets smaller each time
	  * @param h the histogram for this recursion
	  * @param justNumbers Just the freqency-values of the histogram, so we don't have to map the whole thing on each recursion
	  * @return either the final histogram, or a recursion
	**/
	@tailrec def sumTakePercent(totalInstances: BigInt, h: Vector[(String, Int)], justNumbers: Vector[Int]): Vector[(String, Int)] = {
		val sum: BigInt = justNumbers.sum
		val currentPercent: Double = (sum.toDouble / totalInstances.toDouble) * 100
		if ( currentPercent <= targetPercent ) {
			h.sortBy(_._2).reverse
		} else {
			sumTakePercent( totalInstances, h.tail, justNumbers.tail )
		}
	}


	val t: BigInt = histo.map(_._2).sum
	val h: Vector[(String, Int)] = histo.sortBy(_._2) // we want _ascending_ order!
	val n: Vector[Int] = h.map(_._2) // we don't want to re-map the whole histo each time!
	sumTakePercent( t, h, n) 
}


