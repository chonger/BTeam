package parse

import collection.mutable.HashSet
import generate.Jabberwock

object TestFileRead {
	def main(args : Array[String]) : Unit = {
	  
	  var pcfg = new PCFG with FileBased with PrintablePCFG
	  print("Reading ... ")
	  pcfg.read("/data/trainedGrammar.dump")
	  println("done")
   
	  var sym = pcfg.symbolIDs("PRN")
	  println("PRINTING SPLITS FOR SYMBOL " + pcfg.symbolStrings(sym))
	  println(pcfg.printSplits(sym))
   
	  println("Got " + pcfg.terminalStrings.length + " terminals")
	  println("Got " + pcfg.symbolStrings.length + " NTs")
	  println("Got " + pcfg.rules.size + " rules")
	  println("TERM " + pcfg.lexiconRules.size)
	  //val sss = new HashSet[Tuple2[ParseTypes.Symbol,ParseTypes.Split]]()
	  //sss ++= (pcfg.rules.map(r => (r._1.lhs,r._1.split)))
	  //println("Unique NTs : " + sss.size  ) 
   
	  val jwock = new Jabberwock(pcfg)
	  for{i <- 1 to 50} {println(jwock.speak())}
   
	  -1
	}
}
