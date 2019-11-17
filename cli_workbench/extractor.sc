import scala.io.Source
import edu.holycross.shot.cite._

:load utilities.sc

val cexBoilerplate: String = """
#!cexversion
3.0

#!citelibrary
name#Working on Omega 1.12
urn#urn:cite2:cex:fufolio.2019a:omega_1_12_working
license#CC Share Alike. For details, see more info.


#!ctscatalog
urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang
"""

val lib = loadLibrary("../Master-CEX/lemmatized_homer.cex")
val filterUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001:10")

val tr = lib.textRepository.get

val cat = tr.catalog

val catEntries = cat.entriesForUrn(filterUrn)


val catString: String = cexBoilerplate + catEntries.map( e => e.cex("#")).mkString("\n")

val corp = tr.corpus


val filteredCorp = corp ~~ filterUrn

val ctsDataString = "\n#!ctsdata\n" + filteredCorp.cex("#")

val cex: String = catString + "\n\n" + ctsDataString

saveString(cex, "../Master-CEX/", "iliad_10_lemmata.cex")


