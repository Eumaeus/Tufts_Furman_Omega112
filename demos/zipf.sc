import scala.io.Source
import edu.holycross.shot.cite._

// Contains some useful tidbits

:load utilities.sc

/* ------------------------------
   What would Zipf's Law look like in numbers, ideally?
    ------------------------------ */

val idealZipf: Vector[String] = {
	(1 to 26).toVector.map( t => {
		(1.0 / t.toDouble).toString
	})
}

println("Ideal Zipf Numbers")
for ( z <- idealZipf) println(z)
println("")

// A standard frequency-chart from Wikipedia
//val engLetterFreq: Vector[(Char,Int)] = Vector(('a',8167), ('b', 1492), ('c', 2782), ('d', 4253), ('e', 12702), ('f', 2228), ('g', 2015), ('h', 6094), ('i', 6966), ('j', 153), ('k', 772), ('l', 4025), ('m', 2406), ('n', 6749), ('o', 7507), ('p', 1929), ('q', 95), ('r', 5987), ('s', 6327), ('t', 9056), ('u', 2758), ('v', 978), ('w', 2360), ('x', 150), ('y', 1974), ('z', 74)).sortBy(_._2).reverse
val engLetterFreq: Vector[(Char,Int)] = Vector(('E', 111607), ('M', 30129), ('A', 84966), ('H', 30034), ('R', 75809), ('G', 24705), ('I',75448), ('B', 20720), ('O', 71635), ('F', 18121), ('T', 69509), ('Y', 17779), ('N', 66544), ('W', 12899), ('S', 57351), ('K', 11016), ('L', 54893), ('V', 10074), ('C', 45388), ('X', 2902), ('U', 36308), ('Z', 2722), ('D', 33844), ('J', 1965), ('P', 31671), ('Q', 1962)).map( t => (t._1.toLower, t._2)).sortBy(_._2).reverse


println("\nGeneralized Character Frequency")

for ( n <- 1 to engLetterFreq.size) {
	val r: Double = (engLetterFreq(n - 1)._2.toDouble / engLetterFreq(0)._2.toDouble )
	println(s"${r}")
}

// How Samuel Morse did it…

val morseFreq: Vector[(Char,Int)] = Vector((12000, 'E'), (2500, 'F'), (9000, 'T'), (2000, 'W'), (2000, 'Y'), (8000, 'A'), (8000, 'I'), (8000, 'N'), (8000, 'O'), (8000, 'S'), (1700, 'G'), (1700, 'P'), (6400, 'H'), (1600, 'B'), (6200, 'R'), (1200, 'V'), (4400, 'D'), (800, 'K'), (4000, 'L'), (500, 'Q'), (3400, 'U'), (400, 'J'), (400, 'X'), (3000, 'C'), (3000, 'M'), (200, 'Z')).map( t => (t._2.toLower, t._1)).sortBy(_._2).reverse

println("\nMorse's Character Frequencies")

for ( n <- 1 to morseFreq.size) {
	val r: Double = (morseFreq(n - 1)._2.toDouble / morseFreq(0)._2.toDouble )
	println(s"${r}")
}

// Get some text

//		Where is my text?
val filepath:String = "texts/pride_and_prejudice.txt"
//		Read it into a Vector of Strings
val myLines:Vector[String] = Source.fromFile(filepath).getLines.toVector
//		For now, make it one big String ("obs")
val obs:String = myLines.mkString(" ")


/* ------------------------------
   Does Zipf's Law apply to the characters in our text?
    ------------------------------ */
//		Let's throw out anything that is not an alphabetic character
//		…and remove the spaces, too
val alphabeticObs: String = obs.replaceAll("[^A-Za-z]","").toLowerCase
//		Let's turn this into a Vector of Char
val charVec: Vector[Char] = alphabeticObs.toVector
//		Let's group those by .letter
val groupedChars:Map[Char,Vector[Char]] = charVec.groupBy( c => c)
//		Let's make a histogram…
val charHisto:Vector[(Char,Int)] = {
	groupedChars.toVector.map( c => {
		(c._1, c._2.size)
	}).sortBy(_._2).reverse
}

/* ------------------------------
   What does the rank/frequency ratio look like?
    ------------------------------ */
println("Actual Data")
for ( n <- 1 to charHisto.size ) {
	val r: Double = (charHisto(n - 1)._2.toDouble / charHisto(0)._2.toDouble )
	println(s"${r}")
}

/* ----------------------------------
   Let's try this with words
   ---------------------------------- */

// the value 'punctuation' comes from utilities.sc
val justWords: Vector[String] = obs.toLowerCase.split(punctuation).toVector.filter(_.size > 0)

