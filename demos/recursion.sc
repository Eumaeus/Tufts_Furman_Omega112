import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

def ezGraph( w: String, i: Int, total: Int): Unit = {
	val percent:Double = ((i.toDouble / total.toDouble) * 100).toString.take(5).toDouble
	val dataString = s"${percent} ${w}"
	print(dataString.padTo(13,' '))
	for (c <- (1 to (i/600))) print("*")
	print("\n")
}

def goodFact(n: BigInt): BigInt = {
	@tailrec def factTailRec(runningTotal: BigInt, n: BigInt): BigInt = {
		if (n == 1) runningTotal 
		else factTailRec(n * runningTotal, n - 1)
	}
	factTailRec(1, n)
}

def goodSum(n: BigInt): BigInt = {
	@tailrec def sumTailRec(runningTotal: BigInt, n: BigInt): BigInt = {
		if (n == 0) runningTotal 
		else sumTailRec(n + runningTotal, n - 1)
	}
	sumTailRec(0, n)
}
val filepath:String = "texts/pride_and_prejudice.txt"
val lib = loadLibrary("texts/pob.cex")
val myLines: Vector[String] = {
	lib.textRepository.get.corpus.nodes.map(_.text)
}
//val myLines:Vector[String] = Source.fromFile(filepath).getLines.toVector.filter(_.size > 2)
val t: String = myLines.mkString("\n")

val punc: String = """[“”()\[\]·⸁.…,; "?·!–—⸂⸃]"""

val tokens: Vector[String] = {
	t.split(punc).toVector.map(_.trim.toLowerCase).filter(_.size > 0)
}

val tokMap: Vector[(String, Vector[String])] = tokens.groupBy( t => t).toVector
val tokenHisto: Vector[(String, Int)] = tokMap.map( tm => {
	val tok: String = tm._1
	val ct: Int = tm._2.size
	(tok, ct)
}).sortBy(_._2).reverse

val top25: Vector[(String, Int)] = tokenHisto.slice(0,50)

val calcFigs: Vector[String] = {
	(1 to tokenHisto.size).toVector.map( t => {
		(1.0 / t.toDouble).toString
	})
}

saveStringVec(calcFigs, "texts/", "calculatedZipfFigures.txt")



val realFigs: Vector[String] = {
	(1 to tokenHisto.size).toVector.map( t => {
		(tokenHisto(t-1)._2.toDouble / tokenHisto(0)._2.toDouble ).toString
	})
}
saveStringVec(realFigs, "texts/", "pobFigs.txt")

