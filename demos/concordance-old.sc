import scala.io.Source
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import java.io._
import scala.annotation.tailrec

/**
	* Values
*/

val fullStops:Vector[String] = "; ? . :".split(" ").toVector
val halfStops:Vector[String] = ", ( )".split(" ").toVector
val splitters: String = """[()\[\]·⸁.,; "?·!–—⸂⸃]"""

/**
	* Utility Functions
*/

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveStringVec(sv:Vector[String], filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (s <- sv){
		pw.append(s)
		pw.append("\n")
	}
	pw.close
}

def splitWithSplitter(text: String, puncs: String = splitters): Vector[String] = {
	val regexWithSplitter = s"((?<=${splitters})|(?=${splitters}))"
	text.split(regexWithSplitter).toVector.filter(_.size > 0)
}

def prepareText(text: String): String = {
	// Fix Mr. Ms. Mrs.
	val toFix =   Vector("Mr", "Mrs", "Dr", "etc", "Ms", "[ABCDEFGHJKLMNOPQRSTUVWXYZ]")
	text
}

/**
	* Classes
*/

case class IndexedToken(urn: CtsUrn, token:String, idx:Int)

case class ContextVector(contextVec: Vector[IndexedToken]) {
	def sizeLeft(lemmaIndex: Int, leftIndex: Int ): Int = {
		val leftTokens: Vector[IndexedToken] = this.contextVec.slice(leftIndex, lemmaIndex)
		val leftSize: Int = { leftTokens.map(_.token).mkString.size }
		leftSize
	}	
	def sizeRight(lemmaIndex: Int, rightIndex: Int ): Int = {
		val rightTokens: Vector[IndexedToken] = this.contextVec.slice(lemmaIndex + 1, rightIndex + 1)
		val rightSize: Int = { rightTokens.map(_.token).mkString.size }
		rightSize
	}	
	def lemmaSize(lemmaIndex: Int): Int = this.contextVec(lemmaIndex).token.size	
	def getLeft(lemmaIndex:Int, leftStartIndex:Int = 0): Vector[IndexedToken] = {
		this.contextVec.slice(leftStartIndex,lemmaIndex)
	} 
	def getRight(lemmaIndex:Int, rightEndIndex:Int = (this.contextVec.size)): Vector[IndexedToken] = {
		this.contextVec.slice((lemmaIndex + 1), rightEndIndex)
	} 
}

case class ContextFind(citation: CtsUrn, orig: String, context: String )

val libPath = "texts/jane_austen.cex"
val lib: CiteLibrary = loadLibrary(libPath)
val tr: TextRepository = lib.textRepository.get

val testUrn = CtsUrn("urn:cts:fufolio:austen.pp.fufolio:1")
val testCorpus: Corpus = (tr.corpus ~~ testUrn)

/**
  * Concordance Work
*/

def corpusToIndex( c: Corpus ): Vector[IndexedToken] = {
	c.nodes.flatMap( n => {
		val u: CtsUrn = n.urn
		val t: String = n.text
		splitWithSplitter(t).zipWithIndex.map( e => { 
			val urn = CtsUrn(s"${u}.${e._2}")
			IndexedToken(urn, e._1, e._2)
		})
	})
}

def trimTokensLeft(tv: Vector[IndexedToken], chars: Int): Vector[IndexedToken] = {
		@tailrec def trimTokensLeftAcc(tv: Vector[IndexedToken], chars: Int): Vector[IndexedToken] = {
			if ( tv.map(_.token).mkString.size <= chars ) tv
			else trimTokensLeftAcc( tv.tail, chars)
		}
		trimTokensLeftAcc(tv, chars)
}

def trimTokensRight(tv: Vector[IndexedToken], chars: Int): Vector[IndexedToken] = {
		@tailrec def trimTokensRightAcc(tv: Vector[IndexedToken], chars: Int): Vector[IndexedToken] = {
			if ( tv.map(_.token).mkString.size <= chars ) tv
			else trimTokensRightAcc( tv.take(tv.size - 1), chars)
		}
		trimTokensRightAcc(tv, chars)
}

def concordance(f: String, sentence: String, charLimit:Int = 100): Vector[ContextFind] = {
	val passageText: String = sentence
	val textVec: Vector[IndexedToken] = splitWithSplitter(passageText).zipWithIndex.map( e => IndexedToken(e._1, e._2))
	val cv = ContextVector(textVec)
	val foundOnes: Vector[IndexedToken] = textVec.filter( t => {t.token.toLowerCase == f.toLowerCase })
	val rv: Vector[ContextFind] = {
		foundOnes.map( fo => {
				val orig: String = sentence		
				val citation = s"${fo.idx}"

				// Short Sentence
				if (sentence.size < charLimit ) {
					val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
					val leftString: String = left.map(_.token).mkString
					val right: Vector[IndexedToken] = cv.getRight(fo.idx) // defaults to end
					val rightString: String = right.map(_.token).mkString
					val contextString = s"${leftString}**${fo.token}**${rightString}"
					ContextFind(citation, orig, contextString)

				// Long Sentence
				} else {
					val dl: Int = (charLimit - fo.token.size) / 2	
					val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
					val trimmedLeft: Vector[IndexedToken] = trimTokensLeft(left, dl)
					val trimmedLeftString: String = {
						val s: String = trimmedLeft.map(_.token).mkString
						if (left.size > trimmedLeft.size) s"…${s}"
						else s
					}
					val right: Vector[IndexedToken] = cv.getRight(fo.idx) // defaults to end
					val trimmedRight: Vector[IndexedToken] = trimTokensRight(right, dl)
					val trimmedRightString: String = {
						val s: String = trimmedRight.map(_.token).mkString
						if (right.size > trimmedRight.size ) s"${s}…"
						else s
					}
					val contextString = s"${trimmedLeftString}**${fo.token}**${trimmedRightString}"
					ContextFind(citation, orig, contextString)
				}
		})
	}
	for (r <- rv){
		println(s"${r.citation}: \t ${r.context}")
	}
	rv
}

def concordance2(f: String, sentence: String, charLimit:Int = 100): Vector[ContextFind] = {
	val passageText: String = sentence
	val textVec: Vector[IndexedToken] = splitWithSplitter(passageText).zipWithIndex.map( e => IndexedToken(e._1, e._2))
	val cv = ContextVector(textVec)
	val foundOnes: Vector[IndexedToken] = textVec.filter( t => {t.token.toLowerCase == f.toLowerCase })
	val rv: Vector[ContextFind] = {
		foundOnes.map( fo => {
				val orig: String = sentence		
				val citation = s"${fo.idx}"

				// Short Sentence
				if (sentence.size < charLimit ) {
					val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
					val leftString: String = left.map(_.token).mkString
					val right: Vector[IndexedToken] = cv.getRight(fo.idx) // defaults to end
					val rightString: String = right.map(_.token).mkString
					val contextString = s"${leftString}**${fo.token}**${rightString}"
					ContextFind(citation, orig, contextString)

				// Long Sentence
				} else {
					//Calculate L = (charLimit - lemmaSize)/2
					val dl: Int = (charLimit - fo.token.size) / 2	

					// Are there more than L chars to the left?
					val moreToTheLeft: Boolean = {
						val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
						val trimmedLeft: Vector[IndexedToken] = trimTokensLeft(left, dl)
						left.size > trimmedLeft.size
					}						
					// there are more to the left
					if (moreToTheLeft) {
						//Is there a half-stop to the right, closer than L? (Use the farthest right within L)
						val halfStopRight: Option[IndexedToken] = {
							val right: Vector[IndexedToken] = cv.getRight(fo.idx) // defaults to end
							val trimmedRight: Vector[IndexedToken] = trimTokensRight(right, dl)
							val halfStopsRight: Vector[IndexedToken] = {
								trimmedRight.filter( t => { halfStops.contains(t.token) })
							}
							val tokenOption: Option[IndexedToken] = {
								if (halfStopsRight.size > 0) Some(halfStopsRight.last)
								else None
							}
							tokenOption
						}
						halfStopRight match {
							// There is a half-stop to the right…
							case Some(it) => {
								// Calculate Lr, based on the _farthest_ half-stop to the right that is with L of the lemma
								// Take that to the right
								val right: Vector[IndexedToken] = cv.getRight(fo.idx, it.idx + 1)
								val rightString: String = s"${right.map(_.token).mkString}…"
								// Calculate Limit-left
								val newLeftCharLimit: Int = {
									val sizeRight: Int = right.map(_.token).mkString.size
									val sizeLemma: Int = f.size // f was the original search param
									( charLimit - sizeRight - sizeLemma )	
								}
								// Take that to the left
								val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
								val trimmedLeft: Vector[IndexedToken] = trimTokensLeft(left, newLeftCharLimit)
								val trimmedLeftString: String = {
								val s: String = trimmedLeft.map(_.token).mkString
								if (left.size > trimmedLeft.size) s"…${s}"
								else s
								}
								val contextString = s"${trimmedLeftString}**${fo.token}**${rightString}"
								ContextFind(citation, orig, contextString)
							}
							// There is not a half-stop to the right…	
							case None => {
								val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
								val trimmedLeft: Vector[IndexedToken] = trimTokensLeft(left, dl)
								val trimmedLeftString: String = {
									val s: String = trimmedLeft.map(_.token).mkString
									if (left.size > trimmedLeft.size) s"…${s}"
									else s
								}
								val right: Vector[IndexedToken] = cv.getRight(fo.idx) // defaults to end
								val trimmedRight: Vector[IndexedToken] = trimTokensRight(right, dl)
								val trimmedRightString: String = {
									val s: String = trimmedRight.map(_.token).mkString
									if (right.size > trimmedRight.size ) s"${s}…"
									else s
								}
								val contextString = s"${trimmedLeftString.mkString}**${fo.token}**${trimmedRightString.mkString}"
								ContextFind(citation, orig, contextString)
							}
						}

					// not more to the left
					} else {
						val left: Vector[IndexedToken] = cv.getLeft(fo.idx) // defaults to beginning
						val trimmedLeft: Vector[IndexedToken] = trimTokensLeft(left, dl)
						val trimmedLeftString: String = {
							val s: String = trimmedLeft.map(_.token).mkString
							if (left.size > trimmedLeft.size) s"…${s}"
							else s
						}
						// recalculate right limit
						val rightDl = {
							val leftSize: Int = trimmedLeft.map(_.token).mkString.size
							val lemmaSize: Int = f.size // f was the original search string
							( charLimit - leftSize - lemmaSize )
						}
						val right: Vector[IndexedToken] = cv.getRight(fo.idx) // defaults to end
						val trimmedRight: Vector[IndexedToken] = trimTokensRight(right, rightDl)
						val trimmedRightString: String = {
							val s: String = trimmedRight.map(_.token).mkString
							if (right.size > trimmedRight.size ) s"${s}…"
							else s
						}
						val contextString = s"${trimmedLeftString}**${fo.token}**${trimmedRightString}"
						ContextFind(citation, orig, contextString)
					}
				}
		})
	}
	rv
}

/*

Is the whole sentence <= charLimit? If so, highlight our token and be done.

Calculate L = (charLimit - lemmaSize)/2
Are there more than L chars to the left? 
	Yes:
		Is there a half-stop to the left?
			Yes:
				Is there a half-stop to the right, closer than L? (Use the farthest right within L)
					Yes:
						Calculate Lr, based on the _farthest_ half-stop to the right that is with L of the lemma
						Take that to the right
						Calculate Ll
						Take that to the left
					No:
						Take L to the left and right

			No:
				Take L to the left
				Take L to the right

	No:	
		Take all to the left.	
		Calculate Lr = charLimit - (Ll + lemmaSize)
		Take that many to the right

fullStop left -> Index
halfStops left => Indices

Is the closest left-stop full? If so, 

*/


