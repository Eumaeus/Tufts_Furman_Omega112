import scala.io.Source
import edu.holycross.shot.cite._

// Contains some useful tidbits

:load utilities.sc

object Detangler {

  def main(args: Array[String]) {

  }

	case class IndexedLine(text: String, index: Int, citation: String = "")
	case class IndexedChapter(chapter: String, index: Int)
	case class ChapterRange(chapter: String, fromLine: Int, toLine: Int)

	// The text file to de-tangle
	val fn = "texts/diogenes.txt"

	// An optional prefix to the final citations
	val urn = "urn:cts:greekLit:tlg0004.tlg001.hicks:"

	// Get a vector of strings, skipping blank ones
	val lineVec: Vector[String] = loadFile(fn).filter(_.size > 4)

	// Attach a permanent index number to each
	val indexedLines: Vector[IndexedLine] = lineVec.zipWithIndex.map( l => {
		IndexedLine( text = l._1, index = l._2 )
	})

	def indexDivision(lines: Vector[IndexedLine], marker: String): Vector[IndexedChapter] = {
		lines.filter(_.text.startsWith(marker)).map( il => {
			IndexedChapter( chapter = il.text.replaceAll(s"${marker} +",""), index = il.index )
		})
	}

	def defineRanges(lines: Vector[IndexedLine], divs: Vector[IndexedChapter]): Vector[ChapterRange] = {
		val allButLast: Vector[(Int, Int)] = divs.map(_.index).sliding(2,1).toVector.map( t =>{
			(t.head, t.last )
		})
		val theLast: Vector[(Int, Int)] = {
			val chpt: String = divs.last.chapter
			val fl: Int = divs.last.index
			val tl: Int = lines.size
			Vector((fl, tl))
		}
		val rangeVec : Vector[(Int, Int)]= allButLast ++ theLast
		val chapterNames : Vector[String] = divs.map(_.chapter)
		chapterNames.zip(rangeVec).map( i => {
			ChapterRange(chapter = i._1, fromLine = i._2._1, toLine = i._2._2 )
		})
	}

	def filterByRange(filterVec: Vector[ChapterRange], data: Vector[ChapterRange]): Vector[ChapterRange] = {
		Vector[ChapterRange]()	
	}

	def filterByRange(filterVec: Vector[ChapterRange], data: Vector[IndexedLine]): Vector[IndexedLine] = {
		Vector[IndexedLine]()	
	}

	// Pull out just the book-headings, with their indices
	val indexedBooks: Vector[IndexedChapter] = indexDivision(indexedLines, "XXXXbook")
	val bookRanges: Vector[ChapterRange] = defineRanges(indexedLines, indexedBooks)
	// Pull out just the chapter-headings, with their indices
	val indexedSections: Vector[IndexedChapter] = indexDivision(indexedLines, "XXXXsection")
	val sectionRanges: Vector[ChapterRange] = defineRanges(indexedLines, indexedSections)

	// filter sections by book 
	val sectionsByBook:Vector[Vector[ChapterRange]] = {
		bookRanges.map( br => filterByRange(bookRanges, sectionRanges))
	}
}





