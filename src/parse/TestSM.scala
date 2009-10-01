package parse

import io.Source
import train._
import corpora.treebank.TreebankData

object TestSM {
  def main(args : Array[String]) : Unit = {
    
	print("Reading training file at " + args(0) + " ... ")
    val filedata = Source.fromFile(args(0)).getLines
	val treestrs = (List[String]() /: filedata)((a,b) => if(b.charAt(0) != '(') (a(0) + b) :: a.drop(1) else b :: a)
		
    println("Got " + treestrs.length + " trees")

	val data = treestrs.map(new TreebankData(_))	   
	var pcfg : PCFG with PrintablePCFG = new PreProcess(data).pcfg 
	val smp = new SplitMergeTrainer(data,pcfg,2,5)
	   
	smp.run(Nil)
	   
	-1
  }
}
