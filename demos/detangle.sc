import scala.io.Source
import edu.holycross.shot.cite._

// Contains some useful tidbits

:load utilities.sc

case class IndexedLine(text: String, index: Int)
case class IndexedChapter(chapter: String, index: Int)
case class ChapterRange(chapter: String, fromLine: Int, toLine: Int)

// The text file to de-tangle
val fn = "texts/odyssey.txt"

// An optional prefix to the final citations
val urn = "urn:cts:greekLit:tlg0012.tlg002.murray:"

// Get a vector of strings, skipping blank ones
val lineVec: Vector[String] = loadFile(fn).filter(_.size > 4)

// Attach a permanent index number to each
val indexedLines: Vector[IndexedLine] = lineVec.zipWithIndex.map( l => {
	IndexedLine( text = l._1, index = l._2 )
})

// Pull out just the chapter-headings, with their indices
val indexedChapters: Vector[IndexedChapter] = indexedLines.filter(_.text.startsWith("Book")).map( il => {
	IndexedChapter( chapter = il.text.replaceAll("Book ",""), index = il.index )
})

// Create a list, for each chapter, showing the range of text-lines in that chapter
val chapterRanges: Vector[ChapterRange] = {
	// Capture the index of one chapter-heading paired with the index of the next
	// 			This won't get the last one
	val allButLast: Vector[(Int, Int)] = indexedChapters.map(_.index).sliding(2,1).toVector.map( t =>{
		(t.head, t.last )
	})
	
	// Get the last one, with the index of the last chapter-heading, and
	// (index-of-final-line + 1)
	val theLast: Vector[(Int, Int)] = {
		val chpt: String = indexedChapters.last.chapter
		val fl: Int = indexedChapters.last.index
		val tl: Int = indexedLines.size
		Vector((fl, tl))
	}

	// join those two
	val rangeVec : Vector[(Int, Int)]= allButLast ++ theLast

	// Zip them up with the chapter titles
	val chapterNames : Vector[String] = indexedChapters.map(_.chapter)
	chapterNames.zip(rangeVec).map( i => {
		ChapterRange(chapter = i._1, fromLine = i._2._1, toLine = i._2._2 )
	})

}

// Match each chapter-range with the lines of text that go with it
val cexLines: String = {
	chapterRanges.map( cr => {
		val chpt: String = cr.chapter
		val fromLine: Int = cr.fromLine
		val toLine: Int = cr.toLine
		val lines: Vector[String] = indexedLines.filter( l => {
				((l.index > fromLine) && (l.index < toLine))
		}).map( l => {
			s"${urn}${chpt}.${l.text}"
		})
		lines
	}).flatten.mkString("\n")
}

saveString(cexLines, "texts/", "odyssey.cex")




