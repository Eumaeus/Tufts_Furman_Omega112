import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

val lib = loadLibrary("texts/pob.cex")
val tr = lib.textRepository.get

def groupCorpusByText(c:Corpus):Vector[Corpus] = {
	// associate each citable node with its version- or exemplar-level URN
	val vcn: Vector[(CtsUrn, CitableNode)] = c.nodes.map(cn => {
		(cn.urn.dropPassage, cn)
	})
	// adding an index; group by the version- or exemplar-level urn
	val v2: Vector[(CtsUrn,Vector[((CtsUrn,CitableNode), Int)])] = vcn.zipWithIndex.groupBy( n => n._1._1).toVector
	// sort by the saved index number
	val v3: Vector[(CtsUrn,Vector[((CtsUrn,CitableNode), Int)])] = v2.sortBy (_._2.head._2)
	// Drill down to, first, Vector of Vectors of CitableNodes, and then make each a Corpus
	val v4: Vector[Corpus] = {
		val nodeVec:Vector[Vector[CitableNode]] = v3.map(_._2.map( v => {
			v._1._2	
		}))	
		nodeVec.map( nv => Corpus(nv))
	}
	v4
}

