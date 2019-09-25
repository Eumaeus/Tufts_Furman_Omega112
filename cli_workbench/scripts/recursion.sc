import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

def badSum(n: Long): Long = {
	if (n == 0) n
	else n + badSum(n - 1)
}

def goodSum(n: BigInt): BigInt = {
	@tailrec def sumTailRec(runningTotal: BigInt, n: BigInt): BigInt = {
		if (n == 0) runningTotal 
		else sumTailRec(n + runningTotal, n - 1)
	}
	sumTailRec(0, n)
}
